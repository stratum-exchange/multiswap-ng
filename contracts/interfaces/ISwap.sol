// SPDX-License-Identifier: MIT

pragma solidity 0.8.13;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

interface ISwap {
  // pool data view functions
  function getA() external view returns (uint256);

  function getAPrecise() external view returns (uint256);

  function getToken(uint8 index) external view returns (IERC20);

  function getTokensArray() external view returns (address[] memory);

  function getTokenIndex(address tokenAddress) external view returns (uint8);

  function getTokenAddress(uint8 tokenIndex) external view returns (address);

  function getTokenBalance(uint8 index) external view returns (uint256);

  function getVirtualPrice() external view returns (uint256);

  function owner() external view returns (address);

  function paused() external view returns (bool);

  function swapStorage()
    external
    view
    returns (uint256, uint256, uint256, uint256, uint256, uint256, address);

  // min return calculation functions
  function calculateDY(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx
  ) external view returns (uint256);

  function calculateSwap(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx
  ) external view returns (uint256);

  function calculateDX(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dy
  ) external view returns (uint256);

  function calculateTokenAmount(
    uint256[] calldata amounts,
    bool deposit
  ) external view returns (uint256);

  function calculateRemoveLiquidity(
    uint256 amount
  ) external view returns (uint256[] memory);

  function calculateRemoveLiquidityOneToken(
    uint256 tokenAmount,
    uint8 tokenIndex
  ) external view returns (uint256 availableTokenAmount);

  function getAdminBalance(uint256 index) external view returns (uint256);

  // state modifying functions

  function swap(
    uint8 tokenIndexFrom,
    uint8 tokenIndexTo,
    uint256 dx,
    uint256 minDy,
    uint256 deadline
  ) external returns (uint256);

  function addLiquidity(
    uint256[] calldata amounts,
    uint256 minToMint,
    uint256 deadline
  ) external returns (uint256);

  function removeLiquidity(
    uint256 amount,
    uint256[] calldata minAmounts,
    uint256 deadline
  ) external returns (uint256[] memory);

  function removeLiquidityOneToken(
    uint256 tokenAmount,
    uint8 tokenIndex,
    uint256 minAmount,
    uint256 deadline
  ) external returns (uint256);

  function removeLiquidityImbalance(
    uint256[] calldata amounts,
    uint256 maxBurnAmount,
    uint256 deadline
  ) external returns (uint256);

  function getAdminFees() external view returns (uint[] memory);

  function withdrawAdminFees() external;
}
