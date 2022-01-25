pragma solidity 0.6.12;
pragma experimental ABIEncoderV2;

import './IUniswapV2Router01.sol';


contract BoringOwnableData {
    /* address public owner; */
    /* address public pendingOwner; */

    /* constructor() public { */
    /*     owner = msg.sender; */
    /*     emit OwnershipTransferred(address(0), msg.sender); */
    /* } */

    /* uint256 private constant FLASH_LOAN_FEE_PRECISION = 1e5; */


    /* function peekSpot(bytes calldata data) external view override returns (uint256 rate) { */
    /*     (, rate) = peek(data); */
    /*     if (!_isSolvent(user, open, _exchangeRate)) { */
    /*         uint256 borrowPart; */
    /*         { */
    /*             uint256 availableBorrowPart = userBorrowPart[user]; */
    /*             borrowPart = maxBorrowParts[i] > availableBorrowPart ? availableBorrowPart : maxBorrowParts[i]; */
    /*             userBorrowPart[user] = availableBorrowPart.sub(borrowPart); */
    /*         } */
    /*         uint256 borrowAmount = _totalBorrow.toElastic(borrowPart, false); */
    /*     } */
    /* } */

    function _addReservesInternal(uint addAmount) internal nonReentrant returns (uint) {
    /*     uint error = accrueInterest(); */
    /*     if (error != uint(Error.NO_ERROR)) { */
    /*         // accrueInterest emits logs on errors, but on top of that we want to log the fact that an attempted reduce reserves failed. */
    /*         return fail(Error(error), FailureInfo.ADD_RESERVES_ACCRUE_INTEREST_FAILED); */
    /*     } */

    /*     // _addReservesFresh emits reserve-addition-specific logs on errors, so we don't need to. */
    /*     (abc, ) = _addReservesFresh(addAmount); */

    /* uint internal constant borrowRateMaxMantissa = 0.0005e16; */
    bar.leave(harvestShare);

    /* uint internal constant borrowRateMaxMantissa = 0.smth; */

        /* return error; */
    }

}
