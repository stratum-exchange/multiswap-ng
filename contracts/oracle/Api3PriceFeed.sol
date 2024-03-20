// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

import "../interfaces/IPriceFeed.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";
import "@api3/contracts/v0.8/interfaces/IProxy.sol";

/*
 * The Api3PriceFeed uses Nodary as primary oracle. Returns 18 digits.
 */
contract Api3PriceFeed is IPriceFeed {

  using SafeMath for uint256;

  IProxy public proxy;
  bytes32 public priceId;

  // Maximum time period allowed since Api3's latest round data timestamp, beyond which Api3 is considered frozen.
  // For stablecoins we recommend 90000, as Api3 updates once per day when there is no significant price movement
  // For volatile assets we recommend 14400 (4 hours)
  uint256 public immutable TIMEOUT;

  // Maximum deviation allowed between two consecutive Api3 oracle prices. 18-digit precision.
  uint256 public constant MAX_PRICE_DEVIATION_FROM_PREVIOUS_ROUND = 5e17; // 50%

  /*
   * The maximum relative price difference between two oracle responses allowed in order for the PriceFeed
   * to return to using the Api3 oracle. 18-digit precision.
   */
  uint256 public constant MAX_PRICE_DIFFERENCE_BETWEEN_ORACLES = 5e16; // 5%

  // The last good price seen from an oracle
  uint256 public lastGoodPrice;
  // The last good timestamp seen from an oracle
  uint256 public lastGoodTs;

  struct OracleResponse {
    uint256 answer;
    uint256 timestamp;
    bool success;
  }

  enum Status {
    oracleWorking,
    oracleUntrusted
  }

  // The current status of the PriceFeed, which determines the conditions for the next price fetch attempt
  Status public status;

  event LastGoodPriceUpdated(uint256 lastGoodPrice, uint256 prevGoodPrice, uint256 deltaT);
  event PriceFeedStatusChanged(Status newStatus);

  // --- Dependency setters ---

  constructor(IProxy _proxy, bytes32 _priceId, uint256 _timeout) {
    proxy = _proxy;
    priceId = _priceId;
    TIMEOUT = _timeout;

    // Explicitly set initial system status
    status = Status.oracleWorking;

    // Get an initial price from Api3 to serve as first reference for lastGoodPrice
    OracleResponse memory response = _getCurrentResponse();

    require(
      !_oracleIsBroken(response) &&
        block.timestamp.sub(response.timestamp) < _timeout,
      "PriceFeed: Api3 must be working and current"
    );

    lastGoodPrice = uint256(response.answer);
  }

  // --- Functions ---

  /*
   * fetchPrice():
   * Returns the latest price obtained from the Oracle. Called by Stableswap functions that require a current price.
   *
   * Also callable by anyone externally.
   *
   * Non-view function - it stores the last good price.
   *
   * Uses a main oracle (Api3). If it fails,
   * it uses the last good price.
   *
   */
  function fetchPrice() external view override returns (uint256) {
    (, uint256 price, uint256 timestamp) = _fetchPrice();
    require(block.timestamp.sub(timestamp) < TIMEOUT, "oracle is untrusted");
    return price;
  }

  function updatePrice() external override returns (uint256) {
    (Status newStatus, uint256 price, uint256 timestamp) = _fetchPrice();
    require(block.timestamp.sub(timestamp) < TIMEOUT, "oracle is untrusted");
    if (lastGoodPrice != price || lastGoodTs != timestamp) {
      emit LastGoodPriceUpdated(price, lastGoodPrice, timestamp - lastGoodTs);
      lastGoodPrice = price;
      lastGoodTs = timestamp;
    }
    if (status != newStatus) {
      status = newStatus;
      emit PriceFeedStatusChanged(newStatus);
    }
    return price;
  }

  function _fetchPrice() internal view returns (Status, uint256, uint256) {
    // Get current and previous price data from Api3, and current price data from Band
    OracleResponse memory response = _getCurrentResponse();

    // --- CASE 1: System fetched last price from Api3  ---
    if (status == Status.oracleWorking) {
      // If Api3 is broken or frozen
      if (_oracleIsBroken(response) || _oracleIsFrozen(response)) {
        // If Api3 is broken, switch to Band and return current Band price
        return (Status.oracleUntrusted, lastGoodPrice, lastGoodTs);
      }

      // If Api3 is working, return Api3 current price (no status change)
      return (Status.oracleWorking, response.answer, response.timestamp);
    }

    // --- CASE 2: Api3 oracle is untrusted at the last price fetch ---
    if (status == Status.oracleUntrusted) {
      if (_oracleIsBroken(response) || _oracleIsFrozen(response)) {
        return (Status.oracleUntrusted, lastGoodPrice, lastGoodTs);
      }

      return (Status.oracleWorking, response.answer, response.timestamp);
    }
  }

  // --- Helper functions ---

  /* Api3 is considered broken if its current or previous round data is in any way bad. We check the previous round
   * for two reasons:
   *
   * 1) It is necessary data for the price deviation check in case 1,
   * and
   * 2) Api3 is the PriceFeed's preferred primary oracle - having two consecutive valid round responses adds
   * peace of mind when using or returning to Api3.
   */
  function _oracleIsBroken(
    OracleResponse memory _response
  ) internal view returns (bool) {
    // Check for response call reverted
    if (!_response.success) {
      return true;
    }
    // Check for an invalid timeStamp that is 0, or in the future
    if (_response.timestamp == 0 || _response.timestamp > block.timestamp) {
      return true;
    }
    // Check for non-positive price (original value returned from Api3 is int256)
    if (int256(_response.answer) <= 0) {
      return true;
    }

    return false;
  }

  function _oracleIsFrozen(
    OracleResponse memory _response
  ) internal view returns (bool) {
    return block.timestamp.sub(_response.timestamp) > TIMEOUT;
  }
  
  // --- Oracle response wrapper functions ---

  function _getCurrentResponse()
    internal
    view
    returns (OracleResponse memory response)
  {
    // Try to get latest price data:
    try IProxy(proxy).read() returns (
      int224 price,
      uint32 timestamp
    ) {
      // If call to Api3 succeeds, return the response and success = true
      response.answer = uint256(int256(price));
      response.timestamp = uint256(timestamp);
      response.success = true; 
      return response;
    } catch {
      // If call to Api3 aggregator reverts, return a zero response with success = false
      return response;
    }
  }

  function getCurrentResponse()
    external
    view
  returns (OracleResponse memory response) {
    return _getCurrentResponse();
  }
  
  function isHealthy() external view returns (bool) {
    return status == Status.oracleWorking;
  }
}
