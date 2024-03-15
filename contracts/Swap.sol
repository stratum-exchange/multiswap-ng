// SPDX-License-Identifier: UNLICENSED

pragma solidity 0.8.13;

import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/interfaces/IERC4626.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";
import "contracts/interfaces/IPriceFeed.sol";
import "./OwnerPausable.sol";
import "./SwapUtils.sol";
import "./AmplificationUtils.sol";

/**
 * @title Swap - A StableSwap implementation in solidity. Inspired by Saddle Finance and CurveStableSwapNG.vy
 * @notice This contract is responsible for custody of closely pegged assets (eg. group of stablecoins)
 * and automatic market making system. Users become an LP (Liquidity Provider) by depositing their tokens
 * in desired ratios for an exchange of the pool token that represents their share of the pool.
 * Users can burn pool tokens and withdraw their share of token(s).
 *
 * Each time a swap between the pooled tokens happens, a set fee incurs which effectively gets
 * distributed to the gauge voters.
 *
 * In case of emergencies, admin can pause additional deposits, swaps, or single-asset withdraws - which
 * stops the ratio of the tokens in the pool from changing.
 * Users can always withdraw their tokens via multi-asset withdraws.
 *
 * @dev Most of the logic is stored as a library `SwapUtils` for the sake of reducing contract's
 * deployment size.
 * Asset Types:
        0. Standard ERC20 token with no additional features. (e.g. WETH)
                          Note: Users are advised to do careful due-diligence on
                                ERC20 tokens that they interact with, as this
                                contract cannot differentiate between harmless and
                                malicious ERC20 tokens.
        1. Oracle - token with rate oracle (e.g. wstETH)
                    Note: Oracles may be controlled externally by an EOA. Users
                          are advised to proceed with caution.
        2. Rebasing - token with rebase (e.g. stETH). 
                    Note: In this implementation rebase is distributed as fees and not accumulating to LPs. 
        3. ERC4626 - token with convertToAssets method (e.g. sDAI).
                     Note: Some ERC4626 implementations may be susceptible to
                           Donation/Inflation attacks. Users are advised to
                           proceed with caution.
        NOTE: Pool Cannot support tokens with multiple asset types: e.g. ERC4626
              with fees are not supported.
 */
contract Swap is OwnerPausable, ReentrancyGuard {
  using SafeERC20 for IERC20;
  using SafeMath for uint256;
  using SwapUtils for SwapUtils.Swap;
  using AmplificationUtils for SwapUtils.Swap;

  // Struct storing data responsible for automatic market maker functionalities. In order to
  // access this data, this contract uses SwapUtils library. For more details, see SwapUtils.sol
  SwapUtils.Swap public swapStorage;

  // Maps token address to an index in the pool. Used to prevent duplicate tokens in the pool.
  // getTokenIndex function also relies on this mapping to retrieve token index.
  mapping(address => uint8) private tokenIndexes;

  address public rebaseHandler;
  bool initialized;

  /*** EVENTS ***/

  // events replicated from SwapUtils to make the ABI easier for dumb
  // clients
  event TokenSwap(
    address indexed buyer,
    uint256 tokensSold,
    uint256 tokensBought,
    uint128 soldId,
    uint128 boughtId
  );
  event AddLiquidity(
    address indexed provider,
    uint256[] tokenAmounts,
    uint256[] fees,
    uint256 invariant,
    uint256 lpTokenSupply
  );
  event RemoveLiquidity(
    address indexed provider,
    uint256[] tokenAmounts,
    uint256 lpTokenSupply
  );
  event RemoveLiquidityOne(
    address indexed provider,
    uint256 lpTokenAmount,
    uint256 lpTokenSupply,
    uint256 boughtId,
    uint256 tokensBought
  );
  event RemoveLiquidityImbalance(
    address indexed provider,
    uint256[] tokenAmounts,
    uint256[] fees,
    uint256 invariant,
    uint256 lpTokenSupply
  );
  event NewAdminFee(uint256 newAdminFee);
  event NewSwapFee(uint256 newSwapFee);
  event NewWithdrawFee(uint256 newWithdrawFee);
  event RampA(
    uint256 oldA,
    uint256 newA,
    uint256 initialTime,
    uint256 futureTime
  );
  event StopRampA(uint256 currentA, uint256 time);

  /**
   * @notice Initializes this Swap contract with the given parameters.
   * This will also clone a LPToken contract that represents users'
   * LP positions. The owner of LPToken will be this contract - which means
   * only this contract is allowed to mint/burn tokens.
   *
   * @param _pooledTokens an array of ERC20s this pool will accept
   * @param _decimals the decimals to use for each pooled token,
   * eg 8 for WBTC. Cannot be larger than POOL_PRECISION_DECIMALS
   * @param lpTokenName the long-form name of the token to be deployed
   * @param lpTokenSymbol the short symbol for the token to be deployed
   * @param _a the amplification coefficient * n * (n - 1). See the
   * StableSwap paper for details
   * @param _fee default swap fee to be initialized with
   */
  constructor(
    IERC20[] memory _pooledTokens,
    uint8[] memory _decimals,
    string memory lpTokenName,
    string memory lpTokenSymbol,
    uint256 _a,
    uint256 _fee
  ) {
    // Check _pooledTokens and precisions parameter
    require(_pooledTokens.length > 1, "_pooledTokens.length <= 1");
    require(_pooledTokens.length <= 8, "_pooledTokens.length > 8");
    require(
      _pooledTokens.length == _decimals.length,
      "_pooledTokens decimals mismatch"
    );

    uint256[] memory precisionMultipliers = new uint256[](_decimals.length);

    for (uint8 i = 0; i < _pooledTokens.length; i++) {
      if (i > 0) {
        // Check if index is already used. Check if 0th element is a duplicate.
        require(
          tokenIndexes[address(_pooledTokens[i])] == 0 &&
            _pooledTokens[0] != _pooledTokens[i],
          "Duplicate tokens"
        );
      }
      require(
        address(_pooledTokens[i]) != address(0),
        "The 0 address isn't an ERC-20"
      );
      require(
        _decimals[i] <= SwapUtils.POOL_PRECISION_DECIMALS,
        "Token decimals exceeds max"
      );
      precisionMultipliers[i] = 10 ** uint256(36).sub(uint256(_decimals[i]));
      tokenIndexes[address(_pooledTokens[i])] = i;
    }

    // Check _a, _fee, _adminFee, _withdrawFee parameters
    require(_a < AmplificationUtils.MAX_A, "_a exceeds maximum");
    require(_fee <= SwapUtils.MAX_SWAP_FEE, "_fee exceeds maximum");

    // Clone and initialize a LPToken contract
    address _lpToken = address(new LPToken(lpTokenName, lpTokenSymbol));
    LPToken lpToken = LPToken(_lpToken);

    // Initialize swapStorage struct
    swapStorage.lpToken = lpToken;
    swapStorage.pooledTokens = _pooledTokens;
    swapStorage.tokenPrecisionMultipliers = precisionMultipliers;
    swapStorage.balances = new uint256[](_pooledTokens.length);
    swapStorage.initialA = _a.mul(AmplificationUtils.A_PRECISION);
    swapStorage.futureA = _a.mul(AmplificationUtils.A_PRECISION);
    // swapStorage.initialATime = 0;
    // swapStorage.futureATime = 0;
    swapStorage.swapFee = _fee;
  }

  /**
   * @notice Initializes this Swap contract with the given parameters.
   * Constructor stack got too deep.
   * @param _ma_expTime Averaging window of oracle. Set as time_in_seconds / ln(2).
   *  Example: for 10 minute EMA, _ma_exp_time is 600 / ln(2) ~= 866
   * @param _assetTypes Array of uint8 representing tokens in pool
   * @param _decimals the decimals to use for each pooled token,
   * eg 8 for WBTC. Cannot be larger than POOL_PRECISION_DECIMALS
   * @param _oracles Array of rate oracle addresses. Oracle precision must be 10**18.
   */
  function initialize(
    uint256 _ma_expTime,
    uint8[] memory _assetTypes,
    uint8[] memory _decimals,
    IPriceFeed[] memory _oracles
  ) external onlyOwner {
    require(!initialized, "initialized");
    require(_ma_expTime != 0, "Averaging window oracle is zero");
    require(_assetTypes.length == swapStorage.pooledTokens.length && _assetTypes.length == _oracles.length, "length mismatch");

    IERC20[] memory _pooledTokens = swapStorage.pooledTokens;

    uint256[] memory _callAmount = new uint256[](_pooledTokens.length);
    uint256[] memory _scaleFactor = new uint256[](_pooledTokens.length);
    uint256[2][] memory _lastPrices = new uint256[2][](_pooledTokens.length);

    for (uint8 i = 0; i < _pooledTokens.length; i++) {
      if (i < _pooledTokens.length - 1) {
        _lastPrices[i] = [uint256(10 ** 18), uint256(10 ** 18)];
      }
      if (_assetTypes[i] == 3) {
        _callAmount[i] = 10 ** uint256(_decimals[i]);
        address underlyingAsset = IERC4626(address(_pooledTokens[i])).asset();
        _scaleFactor[i] =
          10 **
            uint256(SwapUtils.POOL_PRECISION_DECIMALS).sub(
              uint256(ERC20(underlyingAsset).decimals())
            );
      } else {
        _callAmount[i] = 0;
        _scaleFactor[i] = 0;
      }

      swapStorage.assetTypes = _assetTypes;
      swapStorage.ma_expTime = _ma_expTime;
      swapStorage.d_ma_time = 62324; // 12h in seconds / ln(2)
      swapStorage.ma_lastTime = [block.timestamp, block.timestamp];
      swapStorage.callAmount = _callAmount;
      swapStorage.scaleFactor = _scaleFactor;
      swapStorage.lastPrices = _lastPrices;
      swapStorage.oracles = _oracles;
      initialized = true;
    }
  }

  /*** MODIFIERS ***/

  /**
   * @notice Modifier to check deadline against current timestamp
   * @param deadline latest timestamp to accept this transaction
   */
  modifier deadlineCheck(uint256 deadline) {
    require(block.timestamp <= deadline, "Deadline not met");
    _;
  }

  modifier onlyOwnerOrRebaseHandler() {
    require(msg.sender == owner() || msg.sender == rebaseHandler);
    _;
  }

  /*** VIEW FUNCTIONS ***/

  /**
   * @notice Return A, the amplification coefficient * n * (n - 1)
   * @dev See the StableSwap paper for details
   * @return A parameter
   */
  function getA() external view virtual returns (uint256) {
    return swapStorage.getA();
  }

  /**
   * @notice Return A in its raw precision form
   * @dev See the StableSwap paper for details
   * @return A parameter in its raw precision form
   */
  function getAPrecise() external view virtual returns (uint256) {
    return swapStorage.getAPrecise();
  }

  /**
   * @notice Return address of the pooled token at given index. Reverts if tokenIndex is out of range.
   * @param index the index of the token
   * @return address of the token at given index
   */
  function getToken(uint8 index) public view virtual returns (IERC20) {
    require(index < swapStorage.pooledTokens.length, "Out of range");
    return swapStorage.pooledTokens[index];
  }

  /**
   * @notice Return an array of token contract addresses.
   * @return tokensArray array of token contract addresses.
   */
  function getTokensArray() public view returns (address[] memory) {
    address[] memory tokensArray = new address[](
      swapStorage.pooledTokens.length
    );
    for (uint8 i; i < swapStorage.pooledTokens.length; i++) {
      tokensArray[i] = (address(getToken(i)));
    }

    return tokensArray;
  }

  /**
   * @notice Return the index of the given token address. Reverts if no matching
   * token is found.
   * @param tokenAddress address of the token
   * @return the index of the given token address
   */
  function getTokenIndex(
    address tokenAddress
  ) public view virtual returns (uint8) {
    uint8 index = tokenIndexes[tokenAddress];
    require(address(getToken(index)) == tokenAddress, "Token does not exist");
    return index;
  }

  function getTokenAddress(
    uint8 tokenIndex
  ) public view virtual returns (address) {
    address[] memory tokensArray = getTokensArray();
    return tokensArray[tokenIndex];
  }

  /**
   * @notice Return current balance of the pooled token at given index
   * @param index the index of the token
   * @return current balance of the pooled token at given index with token's native precision
   */
  function getTokenBalance(
    uint8 index
  ) external view virtual returns (uint256) {
    require(index < swapStorage.pooledTokens.length, "Index out of range");
    return swapStorage.balances[index];
  }

  /**
   * @notice Get the virtual price, to help calculate profit
   * @return the virtual price, scaled to the POOL_PRECISION_DECIMALS
   */
  function getVirtualPrice() external view virtual returns (uint256) {
    return swapStorage.getVirtualPrice();
  }

  /**
   * @notice Calculate amount of tokens you receive on swap
   * @param tokenIndexFrom the token the user wants to sell
   * @param tokenIndexTo the token the user wants to buy
   * @param dx the amount of tokens the user wants to sell. If the token charges
   * a fee on transfers, use the amount that gets transferred after the fee.
   * @return amount of tokens the user will receive
   */
  function calculateDY(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx
  ) external view virtual returns (uint256) {
    return swapStorage.calculateDY(tokenIndexFrom, tokenIndexTo, dx);
  }

  /**
   * @notice Same as above, supports FE
   * @param tokenIndexFrom the token the user wants to sell
   * @param tokenIndexTo the token the user wants to buy
   * @param dx the amount of tokens the user wants to sell. If the token charges
   * a fee on transfers, use the amount that gets transferred after the fee.
   * @return amount of tokens the user will receive
   */
  function calculateSwap(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx
  ) external view virtual returns (uint256) {
    return swapStorage.calculateDY(tokenIndexFrom, tokenIndexTo, dx);
  }

  function calculateDX(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dy
  ) external view virtual returns (uint256) {
    return swapStorage.calculateDX(tokenIndexFrom, tokenIndexTo, dy);
  }

  /**
   * @notice A simple method to calculate prices from deposits or
   * withdrawals, excluding fees but including slippage. This is
   * helpful as an input into the various "min" parameters on calls
   * to fight front-running
   *
   * @dev This shouldn't be used outside frontends for user estimates.
   *
   * @param amounts an array of token amounts to deposit or withdrawal,
   * corresponding to pooledTokens. The amount should be in each
   * pooled token's native precision. If a token charges a fee on transfers,
   * use the amount that gets transferred after the fee.
   * @param deposit whether this is a deposit or a withdrawal
   * @return token amount the user will receive
   */
  function calculateTokenAmount(
    uint256[] calldata amounts,
    bool deposit
  ) external view virtual returns (uint256) {
    return swapStorage.calculateTokenAmount(amounts, deposit);
  }

  /**
   * @notice A simple method to calculate amount of each underlying
   * tokens that is returned upon burning given amount of LP tokens
   * @param amount the amount of LP tokens that would be burned on withdrawal
   * @return array of token balances that the user will receive
   */
  function calculateRemoveLiquidity(
    uint256 amount
  ) external view virtual returns (uint256[] memory) {
    return swapStorage.calculateRemoveLiquidity(amount);
  }

  /**
   * @notice Calculate the amount of underlying token available to withdraw
   * when withdrawing via only single token
   * @param tokenAmount the amount of LP token to burn
   * @param tokenIndex index of which token will be withdrawn
   * @return availableTokenAmount calculated amount of underlying token
   * available to withdraw
   */
  function calculateRemoveLiquidityOneToken(
    uint256 tokenAmount,
    uint8 tokenIndex
  ) external view virtual returns (uint256 availableTokenAmount) {
    return swapStorage.calculateWithdrawOneToken(tokenAmount, tokenIndex);
  }

  /**
   * @notice Returns the token balances in the pool. Does not include generated fees/rebase.
   * @return balances array of token balances
   */
  function getBalances() external view returns (uint256[] memory balances) {
    balances = swapStorage.balances;
  }

  /**
   * @notice Gets oracle-based rate multipliers for each coin. If the rate oracle is properly
   * initialized, this function queries that rate by static-calling an external contract.
   * @return rates array of rate multipliers
   */
  function storedRates() external view returns (uint256[] memory rates) {
    rates = swapStorage.getStoredRates();
  }

  /**
   * @notice This function returns the last stored spot price
   * @param index tokenIndex
   * @return spotPrice
   */
  function lastPrice(uint256 index) external view returns (uint256) {
    return swapStorage.lastPrice(index);
  }

  /**
   * @notice This function returns the stored exponential moving average of the spot price.
   * @param index tokenIndex
   * @return emaPrice
   */
  function emaPrice(uint256 index) external view returns (uint256) {
    return swapStorage.emaPrice(index);
  }

  /**
   * @notice This function calculates the current spot price
   * @param index tokenIndex
   * @return p
   */
  function getP(uint256 index) external view virtual returns (uint256) {
    return swapStorage.getP(index);
  }

  /**
   * @notice This function calculates the ema price
   * @param index tokenIndex
   * @return emaPrice
   */
  function priceOracle(uint256 index) external view virtual returns (uint256) {
    return swapStorage.priceOracle(index);
  }

  /**
   * @notice This function calculates the ema D
   * @return d
   */
  function d_Oracle() external view virtual returns (uint256) {
    return swapStorage.d_Oracle();
  }

  /**
   * @notice This function returns the rate oracle helper contract addresses
   * @return oracles
   */
  function oracles() external view virtual returns (IPriceFeed[] memory) {
    return swapStorage.oracles;
  }

  /**
   * @notice This function reads the accumulated amount of admin fees of the token with given index
   * @param index Index of the pooled token
   * @return admin's token balance in the token's precision
   */
  function getAdminBalance(
    uint256 index
  ) external view virtual returns (uint256) {
    return swapStorage.getAdminBalance(index);
  }

  /*** STATE MODIFYING FUNCTIONS ***/

  /**
   * @notice Swap two tokens using this pool
   * @param tokenIndexFrom the token the user wants to swap from
   * @param tokenIndexTo the token the user wants to swap to
   * @param dx the amount of tokens the user wants to swap from
   * @param minDy the min amount the user would like to receive, or revert.
   * @param deadline latest timestamp to accept this transaction
   */
  function swap(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx,
    uint256 minDy,
    uint256 deadline
  )
    external
    virtual
    nonReentrant
    whenNotPaused
    deadlineCheck(deadline)
    returns (uint256)
  {
    return swapStorage.swap(tokenIndexFrom, tokenIndexTo, dx, minDy);
  }

  /**
   * @notice Add liquidity to the pool with the given amounts of tokens
   * @param amounts the amounts of each token to add, in their native precision
   * @param minToMint the minimum LP tokens adding this amount of liquidity
   * should mint, otherwise revert. Handy for front-running mitigation
   * @param deadline latest timestamp to accept this transaction
   * @return amount of LP token user minted and received
   */
  function addLiquidity(
    uint256[] calldata amounts,
    uint256 minToMint,
    uint256 deadline
  )
    external
    virtual
    nonReentrant
    whenNotPaused
    deadlineCheck(deadline)
    returns (uint256)
  {
    return swapStorage.addLiquidity(amounts, minToMint);
  }

  /**
   * @notice Burn LP tokens to remove liquidity from the pool. Withdraw fee that decays linearly
   * over period of 4 weeks since last deposit will apply.
   * @dev Liquidity can always be removed, even when the pool is paused.
   * @param amount the amount of LP tokens to burn
   * @param minAmounts the minimum amounts of each token in the pool
   *        acceptable for this burn. Useful as a front-running mitigation
   * @param deadline latest timestamp to accept this transaction
   * @return amounts of tokens user received
   */
  function removeLiquidity(
    uint256 amount,
    uint256[] calldata minAmounts,
    uint256 deadline
  )
    external
    virtual
    nonReentrant
    deadlineCheck(deadline)
    returns (uint256[] memory)
  {
    return swapStorage.removeLiquidity(amount, minAmounts);
  }

  /**
   * @notice Remove liquidity from the pool all in one token. Withdraw fee that decays linearly
   * over period of 4 weeks since last deposit will apply.
   * @param tokenAmount the amount of the token you want to receive
   * @param tokenIndex the index of the token you want to receive
   * @param minAmount the minimum amount to withdraw, otherwise revert
   * @param deadline latest timestamp to accept this transaction
   * @return amount of chosen token user received
   */
  function removeLiquidityOneToken(
    uint256 tokenAmount,
    uint8 tokenIndex,
    uint256 minAmount,
    uint256 deadline
  )
    external
    virtual
    nonReentrant
    whenNotPaused
    deadlineCheck(deadline)
    returns (uint256)
  {
    return
      swapStorage.removeLiquidityOneToken(tokenAmount, tokenIndex, minAmount);
  }

  /**
   * @notice Remove liquidity from the pool, weighted differently than the
   * pool's current balances. Withdraw fee that decays linearly
   * over period of 4 weeks since last deposit will apply.
   * @param amounts how much of each token to withdraw
   * @param maxBurnAmount the max LP token provider is willing to pay to
   * remove liquidity. Useful as a front-running mitigation.
   * @param deadline latest timestamp to accept this transaction
   * @return amount of LP tokens burned
   */
  function removeLiquidityImbalance(
    uint256[] calldata amounts,
    uint256 maxBurnAmount,
    uint256 deadline
  )
    external
    virtual
    nonReentrant
    whenNotPaused
    deadlineCheck(deadline)
    returns (uint256)
  {
    return swapStorage.removeLiquidityImbalance(amounts, maxBurnAmount);
  }

  /*** ADMIN FUNCTIONS ***/

  /**
   * @notice Set the address for rebaseHandler variable. Usually set to Gauge
   */
  function setRebaseHandler(address _rebaseHandler) external onlyOwner {
    rebaseHandler = _rebaseHandler;
  }

  function getAdminFees() external view virtual returns (uint[] memory) {
    return swapStorage.adminFees();
  }

  /**
   * @notice Withdraw all admin fees to the rebaseHandler (usually gauge)
   */
  function withdrawAdminFees() external virtual nonReentrant {
    require(msg.sender == rebaseHandler, "Not rebaseHandler");
    swapStorage.withdrawAdminFees(rebaseHandler);
  }

  /**
   * @notice Update the swap fee to be applied on swaps
   * @param newSwapFee new swap fee to be applied on future transactions
   */
  function setSwapFee(uint256 newSwapFee) external onlyOwner {
    swapStorage.setSwapFee(newSwapFee);
  }

  /**
   * @notice Start ramping up or down A parameter towards given futureA and futureTime
   * Checks if the change is too rapid, and commits the new A value only when it falls under
   * the limit range.
   * @param futureA the new A to ramp towards
   * @param futureTime timestamp when the new A should be reached
   */
  function rampA(uint256 futureA, uint256 futureTime) external onlyOwner {
    swapStorage.rampA(futureA, futureTime);
  }

  /**
   * @notice Stop ramping A immediately. Reverts if ramp A is already stopped.
   */
  function stopRampA() external onlyOwner {
    swapStorage.stopRampA();
  }

  /**
   * @notice Set the moving average window of the price oracles.
   * @param _ma_expTime Moving average window for the price oracle. It is time_in_seconds / ln(2).
   * @param _d_ma_time Moving average window for the D oracle. It is time_in_seconds / ln(2).
   */
  function setMAExpTime(
    uint256 _ma_expTime,
    uint256 _d_ma_time
  ) external onlyOwner {
    swapStorage.setMAExpTime(_ma_expTime, _d_ma_time);
  }
}
