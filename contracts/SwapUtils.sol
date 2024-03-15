// SPDX-License-Identifier: UNLICENSED

pragma solidity 0.8.13;

import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/interfaces/IERC4626.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";
import "contracts/AmplificationUtils.sol";
import "contracts/LPToken.sol";
import "contracts/MathUtils.sol";
import "contracts/interfaces/IPriceFeed.sol";

/**
 * @title SwapUtils library
 * @notice A library to be used within Swap.sol. Contains functions responsible for custody and AMM functionalities.
 * @dev Contracts relying on this library must initialize SwapUtils.Swap struct then use this library
 * for SwapUtils.Swap struct. Note that this library contains both functions called by users and admins.
 * Admin functions should be protected within contracts using this library.
 */
library SwapUtils {
  using SafeERC20 for IERC20;
  using SafeMath for uint256;
  using MathUtils for uint256;

  /*** EVENTS ***/

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

  struct Swap {
    // variables around the ramp management of A,
    // the amplification coefficient * n * (n - 1)
    // see https://www.curve.fi/stableswap-paper.pdf for details
    uint256 initialA;
    uint256 futureA;
    uint256 initialATime;
    uint256 futureATime;
    // fee calculation
    uint256 swapFee;
    // uint256 adminFee;
    LPToken lpToken;
    // contract references for all tokens being pooled
    IERC20[] pooledTokens;
    // multipliers for each pooled token's precision to get to POOL_PRECISION_DECIMALS
    // for example, TBTC has 18 decimals, so the multiplier should be 1. WBTC
    // has 8, so the multiplier should be 10 ** 18 / 10 ** 8 => 10 ** 10
    uint256[] tokenPrecisionMultipliers;
    // the pool balance of each token, in the token's precision
    // the contract's actual token balance might differ
    uint256[] balances;
    uint8[] assetTypes; // 1=IPriceFeed, 2=1:1 base asset, 3=ERC4626
    uint256 ma_expTime; // averaging window of oracle
    uint256 d_ma_time;
    uint256[2] ma_lastTime; // ma last time P, ma last time D
    uint256[] callAmount; // when using ERC4626 as redemption rate oracle, we ask for conversion rate using this amount.  
    uint256[] scaleFactor;
    uint256[2][] lastPrices; // last price, ma price
    uint256[2] lastD; // last D, ma D
    IPriceFeed[] oracles;
  }

  // Struct storing variables used in calculations in the
  // calculateWithdrawOneTokenDY function to avoid stack too deep errors
  struct CalculateWithdrawOneTokenDYInfo {
    uint256 d0;
    uint256 d1;
    uint256 newY;
    uint256 feePerToken;
    uint256 preciseA;
  }

  // Struct storing variables used in calculations in the
  // {add,remove}Liquidity functions to avoid stack too deep errors
  struct ManageLiquidityInfo {
    uint256 d0;
    uint256 d1;
    uint256 d2;
    uint256 preciseA;
    LPToken lpToken;
    uint256 totalSupply;
    uint256[] balances;
  }

  // the precision all pools tokens will be converted to
  uint8 public constant POOL_PRECISION_DECIMALS = 18;

  // the denominator used to calculate admin and LP fees. For example, an
  // LP fee might be something like tradeAmount.mul(fee).div(FEE_DENOMINATOR)
  uint256 private constant FEE_DENOMINATOR = 10 ** 10;

  // Max swap fee is 1% or 100bps of each swap
  uint256 public constant MAX_SWAP_FEE = 10 ** 8;

  // Max adminFee is 100% of the swapFee
  // adminFee does not add additional fee on top of swapFee
  // Instead it takes a certain % of the swapFee. Therefore it has no impact on the
  // users but only on the earnings of LPs
  uint256 public constant MAX_ADMIN_FEE = 10 ** 10;

  // Constant value used as max loop limit
  uint256 private constant MAX_LOOP_LIMIT = 256;

  uint256 public constant PRECISION = 10 ** 18;

  /*** VIEW & PURE FUNCTIONS ***/

  function _getAPrecise(Swap storage self) internal view returns (uint256) {
    return AmplificationUtils._getAPrecise(self);
  }

  /** @notice Gets rate multipliers for each coin.
    @dev If the coin has a rate oracle that has been properly initialised,
         this method queries that rate by static-calling an external
         contract. */
  function storedRates(
    Swap storage self
  ) internal view returns (uint256[] memory) {
    uint256[] memory rates = self.tokenPrecisionMultipliers;

    for (uint256 i = 0; i < self.pooledTokens.length; i++) {
      if (self.assetTypes[i] == 1 && address(self.oracles[i]) != address(0)) {
        uint256 oracleResponse = self.oracles[i].fetchPrice();
        require(oracleResponse > 0, "failed oracle response");

        rates[i] = rates[i].mul(oracleResponse).div(PRECISION);
      } else if (self.assetTypes[i] == 3) {
        rates[i] = rates[i]
          .mul(
            IERC4626(address(self.pooledTokens[i])).convertToAssets(
              self.callAmount[i].mul(self.scaleFactor[i])
            )
          )
          .div(PRECISION);
      }
    }
    return rates;
  }

  function getStoredRates(
    Swap storage self
  ) external view returns (uint256[] memory) {
    return storedRates(self);
  }
  
  // convert amounts to base asset amounts
  function _xpMem(
    uint256[] memory rates,
    uint256[] memory balances
  ) internal pure returns (uint256[] memory) {
    uint256 numTokens = balances.length;
    require(numTokens == rates.length, "Balances must match rates");
    uint256[] memory result = new uint256[](numTokens);
    for (uint256 i = 0; i < numTokens; i++) {
      result[i] = rates[i].mul(balances[i]).div(PRECISION);
    }
    return result;
  }

  function getDMem(
    uint256[] memory rates,
    uint256[] memory balances,
    uint256 amp
  ) internal pure returns (uint256) {
    uint256[] memory xp = _xpMem(rates, balances);
    return getD(xp, amp);
  }

  function _getP(
    uint256[] memory xp,
    uint256 amp,
    uint256 d
  ) internal pure returns (uint256[] memory) {
    uint256 ann = amp.mul(xp.length);
    uint256 dr = d.div(xp.length ** xp.length);

    for (uint256 i = 0; i < xp.length; i++) {
      dr = dr.mul(d).div(xp[i]);
    }
    uint256[] memory p = new uint256[](xp.length);
    uint256 xp0A = ann.mul(xp[0]).div(AmplificationUtils.A_PRECISION);

    for (uint256 i = 1; i < xp.length; i++) {
      p[i - 1] = uint256(10 ** 18).mul(xp0A.add(dr.mul(xp[0]).div(xp[i]))).div(
        xp0A.add(dr)
      );
    }
    return p;
  }

  function _calculateMovingAverage(
    uint256[2] memory values,
    uint256 averagingWindow,
    uint256 _ma_lastTime
  ) internal view returns (uint256) {
    uint256 lastSpotValue = values[0];
    uint256 lastEMAValue = values[1];

    if (_ma_lastTime < block.timestamp) {
      uint256 alpha = uint256(
        expWad(
          -int256(
            block.timestamp.sub(_ma_lastTime).mul(uint256(10 ** 18)).div(
              averagingWindow
            )
          )
        )
      );
      lastEMAValue = lastSpotValue
        .mul(uint256(10 ** 18).sub(alpha))
        .add(lastEMAValue.mul(alpha))
        .div(uint256(10 ** 18));
    }
    return lastEMAValue;
  }

  
  /** @notice Returns the last stored spot price of token
    @dev if i = 0, it will return the state price of coin[1].
    @param i index of state price (0 for coin[1], 1 for coin[2], ...)
    @return uint256 The last stored spot price */
  function lastPrice(
    Swap storage self,
    uint256 i
  ) external view returns (uint256) {
    return self.lastPrices[i][0];
  }

  /** @notice Returns the last stored ema price of token
    @dev if i = 0, it will return the state price of coin[1].
    @param i index of state price (0 for coin[1], 1 for coin[2], ...)
    @return uint256 The last stored ema price */
  function emaPrice(
    Swap storage self,
    uint256 i
  ) external view returns (uint256) {
    return self.lastPrices[i][1];
  }

  /** @notice Returns the AMM State price of token (calculates the current spot price)
    @dev if i = 0, it will return the state price of coin[1].
    @param i index of state price (0 for coin[1], 1 for coin[2], ...)
    @return uint256 The state price quoted by the AMM for coin[i+1] */
  function getP(Swap storage self, uint256 i) external view returns (uint256) {
    uint256 amp = _getAPrecise(self);
    uint256[] memory xp = _xpMem(storedRates(self), self.balances);
    uint256 d = getD(xp, amp);
    return _getP(xp, amp, d)[i];
  }

  /** @notice Returns the EMA price of token
    @dev if i = 0, it will return the EMA price of coin[1].
    @param i index of state price (0 for coin[1], 1 for coin[2], ...)
    @return uint256 The EMA price calculated */
  function priceOracle(
    Swap storage self,
    uint256 i
  ) external view returns (uint256) {
    return
      _calculateMovingAverage(
        self.lastPrices[i],
        self.ma_expTime,
        self.ma_lastTime[0]
      );
  }

  /** @notice Returns the D invariant EMA 
    @return uint256 The D EMA calculated */
  function d_Oracle(Swap storage self) external view returns (uint256) {
    uint256 ma = _calculateMovingAverage(
      self.lastD,
      self.d_ma_time,
      self.ma_lastTime[1]
    );
    return ma;
  }

  /**
   * @notice Calculate the dy, the amount of selected token that user receives and
   * the fee of withdrawing in one token
   * @param tokenAmount the amount to withdraw in the pool's precision. LP Token
   * burn amount.
   * @param tokenIndex which token will be withdrawn
   * @param self Swap struct to read from
   * @return the amount of token user will receive
   */
  function calculateWithdrawOneToken(
    Swap storage self,
    uint256 tokenAmount,
    uint8 tokenIndex
  ) external view returns (uint256) {
    (uint256 availableTokenAmount, ) = _calculateWithdrawOneToken(
      self,
      tokenAmount,
      tokenIndex,
      self.lpToken.totalSupply()
    );
    return availableTokenAmount;
  }

  function _calculateWithdrawOneToken(
    Swap storage self,
    uint256 tokenAmount,
    uint8 tokenIndex,
    uint256 totalSupply
  ) internal view returns (uint256, uint256) {
    (uint256 dy, uint256 newY, uint256 currentY) = calculateWithdrawOneTokenDY(
      self,
      tokenIndex,
      tokenAmount,
      totalSupply
    );

    // dy_0 (without fees)
    // dy, dy_0 - dy

    uint256 dySwapFee = currentY
      .sub(newY)
      .mul(PRECISION)
      .div(storedRates(self)[tokenIndex])
      .sub(dy);

    return (dy, dySwapFee);
  }

  /**
   * @notice Calculate the dy of withdrawing in one token
   * @param self Swap struct to read from
   * @param tokenIndex which token will be withdrawn
   * @param tokenAmount the amount to withdraw in the pools precision
   * @return the d and the new y after withdrawing one token
   */
  function calculateWithdrawOneTokenDY(
    Swap storage self,
    uint8 tokenIndex,
    uint256 tokenAmount,
    uint256 totalSupply
  ) internal view returns (uint256, uint256, uint256) {
    // Get the current D, then solve the stableswap invariant
    // y_i for D - tokenAmount
    uint256[] memory xp = _xpMem(storedRates(self), self.balances);

    require(tokenIndex < xp.length, "Token index out of range");

    CalculateWithdrawOneTokenDYInfo memory v = CalculateWithdrawOneTokenDYInfo(
      0,
      0,
      0,
      0,
      0
    );
    v.preciseA = _getAPrecise(self);
    v.d0 = getD(xp, v.preciseA);
    v.d1 = v.d0.sub(tokenAmount.mul(v.d0).div(totalSupply));

    require(tokenAmount <= xp[tokenIndex], "Withdraw exceeds available");

    v.newY = getYD(v.preciseA, tokenIndex, xp, v.d1);

    uint256[] memory xpReduced = new uint256[](xp.length);

    v.feePerToken = _feePerToken(self.swapFee, xp.length);
    for (uint256 i = 0; i < xp.length; i++) {
      uint256 xpi = xp[i];
      // if i == tokenIndex, dxExpected = xp[i] * d1 / d0 - newY
      // else dxExpected = xp[i] - (xp[i] * d1 / d0)
      // xpReduced[i] -= dxExpected * fee / FEE_DENOMINATOR
      xpReduced[i] = xpi.sub(
        (
          (i == tokenIndex)
            ? xpi.mul(v.d1).div(v.d0).sub(v.newY)
            : xpi.sub(xpi.mul(v.d1).div(v.d0))
        ).mul(v.feePerToken).div(FEE_DENOMINATOR)
      );
    }

    uint256 dy = xpReduced[tokenIndex].sub(
      getYD(v.preciseA, tokenIndex, xpReduced, v.d1)
    );
    dy = dy.sub(1).mul(PRECISION).div(storedRates(self)[tokenIndex]);

    return (dy, v.newY, xp[tokenIndex]);
  }

  /**
   * @notice Calculate the price of a token in the pool with given
   * precision-adjusted balances and a particular D.
   *
   * @dev This is accomplished via solving the invariant iteratively.
   * See the StableSwap paper and Curve.fi implementation for further details.
   *
   * x_1**2 + x1 * (sum' - (A*n**n - 1) * D / (A * n**n)) = D ** (n + 1) / (n ** (2 * n) * prod' * A)
   * x_1**2 + b*x_1 = c
   * x_1 = (x_1**2 + c) / (2*x_1 + b)
   *
   * @param a the amplification coefficient * n * (n - 1). See the StableSwap paper for details.
   * @param tokenIndex Index of token we are calculating for.
   * @param xp a precision-adjusted set of pool balances. Array should be
   * the same cardinality as the pool.
   * @param d the stableswap invariant
   * @return the price of the token, in the same precision as in xp
   */
  function getYD(
    uint256 a,
    uint8 tokenIndex,
    uint256[] memory xp,
    uint256 d
  ) internal pure returns (uint256) {
    uint256 numTokens = xp.length;
    require(tokenIndex < numTokens, "Token not found");

    uint256 c = d;
    uint256 s;
    uint256 nA = a.mul(numTokens);

    for (uint256 i = 0; i < numTokens; i++) {
      if (i != tokenIndex) {
        s = s.add(xp[i]);
        c = c.mul(d).div(xp[i].mul(numTokens));
        // If we were to protect the division loss we would have to keep the denominator separate
        // and divide at the end. However this leads to overflow with large numTokens or/and D.
        // c = c * D * D * D * ... overflow!
      }
    }
    c = c.mul(d).mul(AmplificationUtils.A_PRECISION).div(nA.mul(numTokens));

    uint256 b = s.add(d.mul(AmplificationUtils.A_PRECISION).div(nA));
    uint256 yPrev;
    uint256 y = d;
    for (uint256 i = 0; i < MAX_LOOP_LIMIT; i++) {
      yPrev = y;
      y = y.mul(y).add(c).div(y.mul(2).add(b).sub(d));
      if (y.within1(yPrev)) {
        return y;
      }
    }
    revert("Approximation did not converge");
  }

  /**
   * @notice Get D, the StableSwap invariant, based on a set of balances and a particular A.
   * @param xp a precision-adjusted and oracle-adjusted set of pool balances. Array should be
   * the same cardinality as the pool.
   * @param a the amplification coefficient * n * (n - 1) in A_PRECISION.
   * See the StableSwap paper for details
   * @return the invariant, at the precision of the pool
   */
  function getD(
    uint256[] memory xp,
    uint256 a
  ) internal pure returns (uint256) {
    uint256 numTokens = xp.length;
    uint256 s;
    for (uint256 i = 0; i < numTokens; i++) {
      s = s.add(xp[i]);
    }
    if (s == 0) {
      return 0;
    }

    uint256 prevD;
    uint256 d = s;
    uint256 nA = a.mul(numTokens);

    for (uint256 i = 0; i < MAX_LOOP_LIMIT; i++) {
      uint256 dP = d;
      for (uint256 j = 0; j < numTokens; j++) {
        dP = dP.mul(d).div(xp[j].mul(numTokens));
        // If we were to protect the division loss we would have to keep the denominator separate
        // and divide at the end. However this leads to overflow with large numTokens or/and D.
        // dP = dP * D * D * D * ... overflow!
      }
      prevD = d;
      d = nA
        .mul(s)
        .div(AmplificationUtils.A_PRECISION)
        .add(dP.mul(numTokens))
        .mul(d)
        .div(
          nA
            .sub(AmplificationUtils.A_PRECISION)
            .mul(d)
            .div(AmplificationUtils.A_PRECISION)
            .add(numTokens.add(1).mul(dP))
        );
      if (d.within1(prevD)) {
        return d;
      }
    }

    // Convergence should occur in 4 loops or less. If this is reached, there may be something wrong
    // with the pool. If this were to occur repeatedly, LPs should withdraw via `removeLiquidity()`
    // function which does not rely on D.
    revert("D does not converge");
  }

  /**
   * @notice Get the virtual price, to help calculate profit
   * @param self Swap struct to read from
   * @return the virtual price, scaled to precision of POOL_PRECISION_DECIMALS
   */
  function getVirtualPrice(Swap storage self) external view returns (uint256) {
    uint256 d = getD(
      _xpMem(storedRates(self), self.balances),
      _getAPrecise(self)
    );
    LPToken lpToken = self.lpToken;
    uint256 supply = lpToken.totalSupply();
    if (supply > 0) {
      return d.mul(10 ** uint256(POOL_PRECISION_DECIMALS)).div(supply);
    }
    return 0;
  }

  /**
   * @notice Calculate the new balances of the tokens given the indexes of the token
   * that is swapped from (FROM) and the token that is swapped to (TO).
   * This function is used as a helper function to calculate how much TO token
   * the user should receive on swap.
   *
   * @param preciseA precise form of amplification coefficient
   * @param tokenIndexFrom index of FROM token
   * @param tokenIndexTo index of TO token
   * @param x the new total amount of FROM token
   * @param xp balances of the tokens in the pool
   * @return the amount of TO token that should remain in the pool
   */
  function getY(
    uint256 preciseA,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 x,
    uint256[] memory xp
  ) internal pure returns (uint256) {
    uint256 numTokens = xp.length;
    require(tokenIndexFrom != tokenIndexTo, "Can't compare token to itself");
    require(
      tokenIndexFrom < numTokens && tokenIndexTo < numTokens,
      "Tokens must be in pool"
    );

    uint256 d = getD(xp, preciseA);
    uint256 c = d;
    uint256 s;
    uint256 nA = numTokens.mul(preciseA);

    uint256 _x;
    for (uint256 i = 0; i < numTokens; i++) {
      if (i == tokenIndexFrom) {
        _x = x;
      } else if (i != tokenIndexTo) {
        _x = xp[i];
      } else {
        continue;
      }
      s = s.add(_x);
      c = c.mul(d).div(_x.mul(numTokens));
      // If we were to protect the division loss we would have to keep the denominator separate
      // and divide at the end. However this leads to overflow with large numTokens or/and D.
      // c = c * D * D * D * ... overflow!
    }
    c = c.mul(d).mul(AmplificationUtils.A_PRECISION).div(nA.mul(numTokens));
    uint256 b = s.add(d.mul(AmplificationUtils.A_PRECISION).div(nA));
    uint256 yPrev;
    uint256 y = d;

    // iterative approximation
    for (uint256 i = 0; i < MAX_LOOP_LIMIT; i++) {
      yPrev = y;
      y = y.mul(y).add(c).div(y.mul(2).add(b).sub(d));
      if (y.within1(yPrev)) {
        return y;
      }
    }
    revert("Approximation did not converge");
  }

  /**
   * @notice Externally calculates a swap between two tokens.
   * @param self Swap struct to read from
   * @param tokenIndexFrom the token to sell
   * @param tokenIndexTo the token to buy
   * @param dx the number of tokens to sell. If the token charges a fee on transfers,
   * use the amount that gets transferred after the fee.
   * @return dy the number of tokens the user will get
   */
  function calculateDY(
    Swap storage self,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx
  ) external view returns (uint256 dy) {
    (dy, , ) = _calculateDY(
      self,
      tokenIndexFrom,
      tokenIndexTo,
      dx,
      self.balances
    );
  }

  /**
   * @notice Internally calculates a swap between two tokens.
   *
   * @dev The caller is expected to transfer the actual amounts (dx and dy)
   * using the token contracts.
   *
   * @param self Swap struct to read from
   * @param tokenIndexFrom the token to sell
   * @param tokenIndexTo the token to buy
   * @param dx the number of tokens to sell. If the token charges a fee on transfers,
   * use the amount that gets transferred after the fee.
   * @return dy the number of tokens the user will get
   * @return dyFee the associated fee
   */
  function _calculateDY(
    Swap storage self,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx,
    uint256[] memory balances
  ) internal view returns (uint256 dy, uint256 dyFee, uint256[] memory newXp) {
    uint256[] memory rates = storedRates(self);
    uint256[] memory xp = _xpMem(rates, balances);
    require(
      tokenIndexFrom < xp.length && tokenIndexTo < xp.length,
      "Token index out of range"
    );
    uint256 x = dx.mul(rates[tokenIndexFrom]).div(PRECISION).add(
      xp[tokenIndexFrom]
    );
    uint256 y = getY(_getAPrecise(self), tokenIndexFrom, tokenIndexTo, x, xp);
    dy = xp[tokenIndexTo].sub(y).sub(1);
    dyFee = dy.mul(self.swapFee).div(FEE_DENOMINATOR);
    dy = dy.sub(dyFee).mul(PRECISION).div(rates[tokenIndexTo]);
    newXp = xp;
    newXp[tokenIndexFrom] = x;
    newXp[tokenIndexTo] = y;
  }

  /**
    @notice Calculate the current input dx given output dy
    @param tokenIndexFrom Index value for the coin to send
    @param tokenIndexTo Index value of the coin to receive
    @param dy Amount of `tokenIndexTo` being received after exchange
    @return dx Amount of `tokenIndexFrom` predicted
   */
  function calculateDX(
    Swap storage self,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dy
  ) external view returns (uint256 dx) {
    dx = _calculateDX(self, tokenIndexFrom, tokenIndexTo, dy, self.balances);
  }

  function _calculateDX(
    Swap storage self,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dy,
    uint256[] memory balances
  ) internal view returns (uint256 dx) {
    uint256[] memory rates = storedRates(self);
    uint256[] memory xp = _xpMem(rates, balances);

    uint256 dyWithFee = dy.mul(rates[tokenIndexTo]).div(PRECISION).add(1);
    uint256 y = xp[tokenIndexTo].sub(
      dyWithFee.mul(FEE_DENOMINATOR).div(FEE_DENOMINATOR.sub(self.swapFee))
    );
    uint256 x = getY(_getAPrecise(self), tokenIndexTo, tokenIndexFrom, y, xp); // reverse order
    dx = x.sub(xp[tokenIndexFrom]).mul(PRECISION).div(rates[tokenIndexFrom]);
  }

  /**
   * @notice A simple method to calculate amount of each underlying
   * tokens that is returned upon burning given amount of
   * LP tokens
   *
   * @param amount the amount of LP tokens that would to be burned on
   * withdrawal
   * @return array of amounts of tokens user will receive
   */
  function calculateRemoveLiquidity(
    Swap storage self,
    uint256 amount
  ) external view returns (uint256[] memory) {
    return
      _calculateRemoveLiquidity(
        self.balances,
        amount,
        self.lpToken.totalSupply()
      );
  }

  function _calculateRemoveLiquidity(
    uint256[] memory balances,
    uint256 amount,
    uint256 totalSupply
  ) internal pure returns (uint256[] memory) {
    require(amount <= totalSupply, "Cannot exceed total supply");

    uint256[] memory amounts = new uint256[](balances.length);

    for (uint256 i = 0; i < balances.length; i++) {
      amounts[i] = balances[i].mul(amount).div(totalSupply);
    }
    return amounts;
  }

  /**
   * @notice A simple method to calculate prices from deposits or
   * withdrawals, excluding fees but including slippage. This is
   * helpful as an input into the various "min" parameters on calls
   * to fight front-running
   *
   * @dev This shouldn't be used outside frontends for user estimates.
   *
   * @param self Swap struct to read from
   * @param amounts an array of token amounts to deposit or withdrawal,
   * corresponding to pooledTokens. The amount should be in each
   * pooled token's native precision. If a token charges a fee on transfers,
   * use the amount that gets transferred after the fee.
   * @param deposit whether this is a deposit or a withdrawal
   * @return if deposit was true, total amount of lp token that will be minted and if
   * deposit was false, total amount of lp token that will be burned
   */
  function calculateTokenAmount(
    Swap storage self,
    uint256[] calldata amounts,
    bool deposit
  ) external view returns (uint256) {
    uint256 a = _getAPrecise(self);
    uint256[] memory balances = self.balances;
    uint256[] memory rates = storedRates(self);

    uint256 d0 = getD(_xpMem(rates, balances), a);
    for (uint256 i = 0; i < balances.length; i++) {
      if (deposit) {
        balances[i] = balances[i].add(amounts[i]);
      } else {
        balances[i] = balances[i].sub(
          amounts[i],
          "Cannot withdraw more than available"
        );
      }
    }
    uint256 d1 = getD(_xpMem(rates, balances), a);
    uint256 totalSupply = self.lpToken.totalSupply();

    if (deposit) {
      return d1.sub(d0).mul(totalSupply).div(d0);
    } else {
      return d0.sub(d1).mul(totalSupply).div(d0);
    }
  }

  /**
   * @notice return accumulated amount of admin fees of the token with given index
   * @param self Swap struct to read from
   * @param index Index of the pooled token
   * @return admin balance in the token's precision
   */
  function getAdminBalance(
    Swap storage self,
    uint256 index
  ) external view returns (uint256) {
    require(index < self.pooledTokens.length, "Token index out of range");
    return
      self.pooledTokens[index].balanceOf(address(this)).sub(
        self.balances[index]
      );
  }

  /**
   * @notice internal helper function to calculate fee per token multiplier used in
   * swap fee calculations
   * @param swapFee swap fee for the tokens
   * @param numTokens number of tokens pooled
   */
  function _feePerToken(
    uint256 swapFee,
    uint256 numTokens
  ) internal pure returns (uint256) {
    return swapFee.mul(numTokens).div(numTokens.sub(1).mul(4));
  }

  /*** STATE MODIFYING FUNCTIONS ***/

  /**
   * @notice swap two tokens in the pool
   * @param self Swap struct to read from and write to
   * @param tokenIndexFrom the token the user wants to sell
   * @param tokenIndexTo the token the user wants to buy
   * @param dx the amount of tokens the user wants to sell
   * @param minDy the min amount the user would like to receive, or revert.
   * @return amount of token user received on swap
   */
  function swap(
    Swap storage self,
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx,
    uint256 minDy
  ) external returns (uint256) {
    {
      IERC20 tokenFrom = self.pooledTokens[tokenIndexFrom];
      require(tokenIndexFrom != tokenIndexTo, "must be different tokens");
      require(dx > 0, "do not exchange 0 tokens");
      require(
        dx <= tokenFrom.balanceOf(msg.sender),
        "Cannot swap more than you own"
      );
      // Transfer tokens first to see if a fee was charged on transfer
      uint256 beforeBalance = tokenFrom.balanceOf(address(this));
      tokenFrom.safeTransferFrom(msg.sender, address(this), dx);

      // Use the actual transferred amount for AMM math
      dx = tokenFrom.balanceOf(address(this)).sub(beforeBalance);
    }
    uint256 dy;
    uint256 dyFee;
    uint256[] memory xp;
    uint256[] memory balances = self.balances;
    (dy, dyFee, xp) = _calculateDY(
      self,
      tokenIndexFrom,
      tokenIndexTo,
      dx,
      balances
    );
    require(dy >= minDy, "Swap didn't result in min tokens");

    uint256 amp = _getAPrecise(self);
    upkeepOracles(self, xp, amp, getD(xp, amp));

    self.balances[tokenIndexFrom] = balances[tokenIndexFrom].add(dx);
    self.balances[tokenIndexTo] = balances[tokenIndexTo].sub(dy).sub(
      dyFee
    );

    self.pooledTokens[tokenIndexTo].safeTransfer(msg.sender, dy);

    emit TokenSwap(msg.sender, dx, dy, tokenIndexFrom, tokenIndexTo);

    return dy;
  }

  /**
   * @notice Add liquidity to the pool
   * @param self Swap struct to read from and write to
   * @param amounts the amounts of each token to add, in their native precision
   * @param minToMint the minimum LP tokens adding this amount of liquidity
   * should mint, otherwise revert. Handy for front-running mitigation
   * allowed addresses. If the pool is not in the guarded launch phase, this parameter will be ignored.
   * @return amount of LP token user received
   */
  function addLiquidity(
    Swap storage self,
    uint256[] memory amounts,
    uint256 minToMint
  ) external returns (uint256) {
    IERC20[] memory pooledTokens = self.pooledTokens;
    require(
      amounts.length == pooledTokens.length,
      "Amounts must match pooled tokens"
    );

    // current state
    ManageLiquidityInfo memory v = ManageLiquidityInfo(
      0,
      0,
      0,
      _getAPrecise(self),
      self.lpToken,
      0,
      self.balances
    );
    v.totalSupply = v.lpToken.totalSupply();
    uint256[] memory rates = storedRates(self);

    if (v.totalSupply != 0) {
      v.d0 = getDMem(rates, v.balances, v.preciseA);
    }

    uint256[] memory newBalances = new uint256[](pooledTokens.length);

    for (uint256 i = 0; i < pooledTokens.length; i++) {
      require(
        v.totalSupply != 0 || amounts[i] > 0,
        "Must supply all tokens in pool"
      );

      // Transfer tokens first to see if a fee was charged on transfer
      if (amounts[i] != 0) {
        uint256 beforeBalance = pooledTokens[i].balanceOf(address(this));
        pooledTokens[i].safeTransferFrom(msg.sender, address(this), amounts[i]);

        // Update the amounts[] with actual transfer amount
        amounts[i] = pooledTokens[i].balanceOf(address(this)).sub(
          beforeBalance
        );
      }

      newBalances[i] = v.balances[i].add(amounts[i]);
    }

    // invariant after change
    v.d1 = getDMem(rates, newBalances, v.preciseA);
    require(v.d1 > v.d0, "D should increase");

    // updated to reflect fees and calculate the user's LP tokens
    v.d2 = v.d1;
    uint256[] memory fees = new uint256[](pooledTokens.length);

    if (v.totalSupply != 0) {
      uint256 feePerToken = _feePerToken(self.swapFee, pooledTokens.length);
      for (uint256 i = 0; i < pooledTokens.length; i++) {
        uint256 idealBalance = v.d1.mul(v.balances[i]).div(v.d0);
        fees[i] = feePerToken.mul(idealBalance.difference(newBalances[i])).div(
          FEE_DENOMINATOR
        );
        self.balances[i] = newBalances[i].sub(
          fees[i].mul(MAX_ADMIN_FEE).div(FEE_DENOMINATOR)
        );
        newBalances[i] = newBalances[i].sub(fees[i]);
      }
      uint256[] memory xp = _xpMem(rates, newBalances);
      v.d2 = getD(xp, v.preciseA);
      upkeepOracles(self, xp, v.preciseA, v.d2);
    } else {
      // the initial depositor doesn't pay fees
      self.balances = newBalances;
    }

    uint256 toMint;
    if (v.totalSupply == 0) {
      toMint = v.d1; // Take the dust if there was any
      self.lastD = [v.d1, v.d1]; // (re)instantiate D oracle if totalSupply is zero.
      if (self.ma_lastTime[1] < block.timestamp) {
        self.ma_lastTime[1] = block.timestamp; // Update D ma time
      }
    } else {
      toMint = v.d2.sub(v.d0).mul(v.totalSupply).div(v.d0);
    }

    require(toMint >= minToMint, "Couldn't mint min requested");

    // mint the user's LP tokens
    v.lpToken.mint(msg.sender, toMint);

    emit AddLiquidity(
      msg.sender,
      amounts,
      fees,
      v.d1,
      v.totalSupply.add(toMint)
    );

    return toMint;
  }

  /**
   * @notice Burn LP tokens to remove liquidity from the pool.
   * @dev Liquidity can always be removed, even when the pool is paused.
   * @param self Swap struct to read from and write to
   * @param amount the amount of LP tokens to burn
   * @param minAmounts the minimum amounts of each token in the pool
   * acceptable for this burn. Useful as a front-running mitigation
   * @return amounts of tokens the user received
   */
  function removeLiquidity(
    Swap storage self,
    uint256 amount,
    uint256[] calldata minAmounts
  ) external returns (uint256[] memory) {
    LPToken lpToken = self.lpToken;
    IERC20[] memory pooledTokens = self.pooledTokens;
    require(amount > 0, "invalid burn amount");
    require(amount <= lpToken.balanceOf(msg.sender), ">LP.balanceOf");
    require(
      minAmounts.length == pooledTokens.length,
      "minAmounts must match poolTokens"
    );

    uint256[] memory balances = self.balances;
    uint256 totalSupply = lpToken.totalSupply();

    uint256[] memory amounts = _calculateRemoveLiquidity(
      balances,
      amount,
      totalSupply
    );

    for (uint256 i = 0; i < amounts.length; i++) {
      require(amounts[i] >= minAmounts[i], "amounts[i] < minAmounts[i]");
      self.balances[i] = balances[i].sub(amounts[i]);
      pooledTokens[i].safeTransfer(msg.sender, amounts[i]);
    }

    lpToken.burnFrom(msg.sender, amount);

    // Upkeep D_oracle

    uint256[2] memory lastDCurrent = self.lastD;
    uint256 d = lastDCurrent[0].sub(
      lastDCurrent[0].mul(amount).div(totalSupply)
    );
    uint256 maD = _calculateMovingAverage(
      lastDCurrent,
      self.d_ma_time,
      self.ma_lastTime[1] // index 1 is ma_last_time for D
    );
    self.lastD = [d, maD];

    if (self.ma_lastTime[1] < block.timestamp) {
      self.ma_lastTime[1] = block.timestamp;
    }

    emit RemoveLiquidity(msg.sender, amounts, totalSupply.sub(amount));

    return amounts;
  }

  /**
   * @notice Remove liquidity from the pool all in one token.
   * @param self Swap struct to read from and write to
   * @param tokenAmount the amount of the lp tokens to burn
   * @param tokenIndex the index of the token you want to receive
   * @param minAmount the minimum amount to withdraw, otherwise revert
   * @return amount chosen token that user received
   */
  function removeLiquidityOneToken(
    Swap storage self,
    uint256 tokenAmount,
    uint8 tokenIndex,
    uint256 minAmount
  ) external returns (uint256) {
    LPToken lpToken = self.lpToken;
    IERC20[] memory pooledTokens = self.pooledTokens;

    require(tokenAmount <= lpToken.balanceOf(msg.sender), ">LP.balanceOf");
    require(tokenIndex < pooledTokens.length, "Token not found");

    uint256 totalSupply = lpToken.totalSupply();

    (uint256 dy, uint256 dyFee) = _calculateWithdrawOneToken(
      self,
      tokenAmount,
      tokenIndex,
      totalSupply
    );

    require(dy >= minAmount, "dy < minAmount");

    self.balances[tokenIndex] = self.balances[tokenIndex].sub(
      dy.add(dyFee.mul(MAX_ADMIN_FEE).div(FEE_DENOMINATOR))
    );
    lpToken.burnFrom(msg.sender, tokenAmount);
    pooledTokens[tokenIndex].safeTransfer(msg.sender, dy);

    emit RemoveLiquidityOne(
      msg.sender,
      tokenAmount,
      totalSupply,
      tokenIndex,
      dy
    );

    uint256[] memory xp = _xpMem(storedRates(self), self.balances);
    uint256 amp = _getAPrecise(self);

    upkeepOracles(self, xp, amp, getD(xp, amp));

    return dy;
  }

  /**
   * @notice Remove liquidity from the pool, weighted differently than the
   * pool's current balances.
   *
   * @param self Swap struct to read from and write to
   * @param amounts how much of each token to withdraw
   * @param maxBurnAmount the max LP token provider is willing to pay to
   * remove liquidity. Useful as a front-running mitigation.
   * @return actual amount of LP tokens burned in the withdrawal
   */
  function removeLiquidityImbalance(
    Swap storage self,
    uint256[] memory amounts,
    uint256 maxBurnAmount
  ) public returns (uint256) {
    ManageLiquidityInfo memory v = ManageLiquidityInfo(
      0,
      0,
      0,
      _getAPrecise(self),
      self.lpToken,
      0,
      self.balances
    );
    v.totalSupply = v.lpToken.totalSupply();
    uint256[] memory rates = storedRates(self);

    IERC20[] memory pooledTokens = self.pooledTokens;

    require(
      amounts.length == pooledTokens.length,
      "Amounts should match pool tokens"
    );

    require(
      maxBurnAmount <= v.lpToken.balanceOf(msg.sender) && maxBurnAmount != 0,
      ">LP.balanceOf"
    );

    uint256 feePerToken = _feePerToken(self.swapFee, pooledTokens.length);
    uint256[] memory fees = new uint256[](pooledTokens.length);
    {
      uint256[] memory balances1 = new uint256[](pooledTokens.length);
      v.d0 = getDMem(rates, v.balances, v.preciseA);
      for (uint256 i = 0; i < pooledTokens.length; i++) {
        balances1[i] = v.balances[i].sub(
          amounts[i],
          "Cannot withdraw more than available"
        );
      }
      v.d1 = getDMem(rates, balances1, v.preciseA);

      for (uint256 i = 0; i < pooledTokens.length; i++) {
        uint256 idealBalance = v.d1.mul(v.balances[i]).div(v.d0);
        uint256 difference = idealBalance.difference(balances1[i]);
        fees[i] = feePerToken.mul(difference).div(FEE_DENOMINATOR);
        self.balances[i] = balances1[i].sub(
          fees[i].mul(MAX_ADMIN_FEE).div(FEE_DENOMINATOR)
        );
        balances1[i] = balances1[i].sub(fees[i]);
      }

      v.d2 = getDMem(rates, balances1, v.preciseA);
      upkeepOracles(self, _xpMem(rates, balances1), v.preciseA, v.d2);
    }
    uint256 tokenAmount = v.d0.sub(v.d2).mul(v.totalSupply).div(v.d0);
    require(tokenAmount != 0, "Burnt amount cannot be zero");
    tokenAmount = tokenAmount.add(1);

    require(tokenAmount <= maxBurnAmount, "tokenAmount > maxBurnAmount");

    v.lpToken.burnFrom(msg.sender, tokenAmount);

    for (uint256 i = 0; i < pooledTokens.length; i++) {
      pooledTokens[i].safeTransfer(msg.sender, amounts[i]);
    }

    emit RemoveLiquidityImbalance(
      msg.sender,
      amounts,
      fees,
      v.d1,
      v.totalSupply.sub(tokenAmount)
    );

    return tokenAmount;
  }

  function adminFees(
    Swap storage self
  ) external view returns (uint256[] memory) {
    IERC20[] memory pooledTokens = self.pooledTokens;
    uint256[] memory fees = new uint256[](pooledTokens.length);
    for (uint256 i = 0; i < pooledTokens.length; i++) {
      IERC20 token = pooledTokens[i];
      uint256 balance = token.balanceOf(address(this)).sub(self.balances[i]);
      if (balance != 0) {
        fees[i] = balance;
      }
    }
    return fees;
  }

  /**
   * @notice withdraw all admin fees to a given address
   * @param self Swap struct to withdraw fees from
   * @param to Address to send the fees to
   */
  function withdrawAdminFees(Swap storage self, address to) external {
    IERC20[] memory pooledTokens = self.pooledTokens;
    for (uint256 i = 0; i < pooledTokens.length; i++) {
      IERC20 token = pooledTokens[i];
      uint256 balance = token.balanceOf(address(this)).sub(self.balances[i]);
      if (balance != 0) {
        token.safeTransfer(to, balance);
      }
    }
  }

  /**
   * @notice update the swap fee
   * @dev fee cannot be higher than 1% of each swap
   * @param self Swap struct to update
   * @param newSwapFee new swap fee to be applied on future transactions
   */
  function setSwapFee(Swap storage self, uint256 newSwapFee) external {
    require(newSwapFee <= MAX_SWAP_FEE, "Fee is too high");
    self.swapFee = newSwapFee;

    emit NewSwapFee(newSwapFee);
  }

  /**@notice Upkeeps price and D oracles. */
  function upkeepOracles(
    Swap storage self,
    uint256[] memory xp,
    uint256 amp,
    uint256 d
  ) internal {
    uint256[2][] memory lastPricesCurrent = self.lastPrices;
    uint256[2][] memory lastPricesNew = lastPricesCurrent;
    uint256[] memory spotPrice = _getP(xp, amp, d);

    // Upkeep external PriceFeed

    uint256 numTokens = xp.length;
    for (uint256 i = 0; i < numTokens; i++) {
      if (self.assetTypes[i] == 1 && address(self.oracles[i]) != address(0)) {
        self.oracles[i].updatePrice();
        require(self.oracles[i].isHealthy(), "oracle is not working");
      }
    }

    // Upkeep price oracle

    for (uint256 i = 0; i < numTokens - 1; i++) {
      if (spotPrice[i] != 0) {
        uint256 lp;
        uint256 ma;
        if (spotPrice[i] < uint256(20 ** 18)) {
          lp = spotPrice[i];
        } else {
          lp = uint256(20 ** 18);
        }
        ma = _calculateMovingAverage(
          lastPricesCurrent[i],
          self.ma_expTime,
          self.ma_lastTime[0]
        );
        lastPricesNew[i] = [lp, ma];
      }
    }
    self.lastPrices = lastPricesNew;

    // Upkeep D oracle

    uint256[2] memory lastDCurrent = self.lastD;
    uint256 maD = _calculateMovingAverage(
      lastDCurrent,
      self.d_ma_time,
      self.ma_lastTime[1] // index 1 is ma_last_time for D
    );
    self.lastD = [d, maD];

    // Housekeeping: Update ma_last_time for p and D oracles

    for (uint256 i = 0; i < 2; i++) {
      if (self.ma_lastTime[i] < block.timestamp) {
        self.ma_lastTime[i] = block.timestamp;
      }
    }
  }

  function setMAExpTime(
    Swap storage self,
    uint256 _ma_expTime,
    uint256 _d_ma_time
  ) external {
    require(_ma_expTime.mul(_d_ma_time) > 0, "0 in input values");
    self.ma_expTime = _ma_expTime;
    self.d_ma_time = _d_ma_time;
  }

  /**  @dev Calculates the natural exponential function of a signed integer with
         a precision of 1e18.
    @notice Remco Bloemen's implementation under the MIT license here:
            https://xn--2-umb.com/22/exp-ln.
    @param x The 32-byte variable.
    @return r The 32-byte calculation result. */
  function expWad(int256 x) internal pure returns (int256 r) {
    unchecked {
      // When the result is < 0.5 we return zero. This happens when
      // x <= floor(log(0.5e18) * 1e18) ~ -42e18
      if (x <= -42139678854452767551) return 0;

      // When the result is > (2**255 - 1) / 1e18 we can not represent it as an
      // int. This happens when x >= floor(log((2**255 - 1) / 1e18) * 1e18) ~ 135.
      if (x >= 135305999368893231589) revert("EXP_OVERFLOW");

      // x is now in the range (-42, 136) * 1e18. Convert to (-42, 136) * 2**96
      // for more intermediate precision and a binary basis. This base conversion
      // is a multiplication by 1e18 / 2**96 = 5**18 / 2**78.
      x = (x << 78) / 5 ** 18;

      // Reduce range of x to (-½ ln 2, ½ ln 2) * 2**96 by factoring out powers
      // of two such that exp(x) = exp(x') * 2**k, where k is an integer.
      // Solving this gives k = round(x / log(2)) and x' = x - k * log(2).
      int256 k = ((x << 96) / 54916777467707473351141471128 + 2 ** 95) >> 96;
      x = x - k * 54916777467707473351141471128;

      // k is in the range [-61, 195].

      // Evaluate using a (6, 7)-term rational approximation.
      // p is made monic, we'll multiply by a scale factor later.
      int256 y = x + 1346386616545796478920950773328;
      y = ((y * x) >> 96) + 57155421227552351082224309758442;
      int256 p = y + x - 94201549194550492254356042504812;
      p = ((p * y) >> 96) + 28719021644029726153956944680412240;
      p = p * x + (4385272521454847904659076985693276 << 96);

      // We leave p in 2**192 basis so we don't need to scale it back up for the division.
      int256 q = x - 2855989394907223263936484059900;
      q = ((q * x) >> 96) + 50020603652535783019961831881945;
      q = ((q * x) >> 96) - 533845033583426703283633433725380;
      q = ((q * x) >> 96) + 3604857256930695427073651918091429;
      q = ((q * x) >> 96) - 14423608567350463180887372962807573;
      q = ((q * x) >> 96) + 26449188498355588339934803723976023;

      assembly {
        // Div in assembly because solidity adds a zero check despite the unchecked.
        // The q polynomial won't have zeros in the domain as all its roots are complex.
        // No scaling is necessary because p is already 2**96 too large.
        r := sdiv(p, q)
      }

      // r should be in the range (0.09, 0.25) * 2**96.

      // We now need to multiply r by:
      // * the scale factor s = ~6.031367120.
      // * the 2**k factor from the range reduction.
      // * the 1e18 / 2**96 factor for base conversion.
      // We do this all at once, with an intermediate result in 2**213
      // basis, so the final right shift is always by a positive amount.
      r = int256(
        (uint256(r) * 3822833074963236453042738258902158003155416615667) >>
          uint256(195 - k)
      );
    }
  }
}
