pragma solidity ^0.8.0;
pragma solidity ^0.8.1;
contract one {
    function alfa () { return; }
}
contract two {
    function beta (uint) { return 1; }
    function gamma (uint, uint) { return 2; }
    function delta (uint, uint, uint) { return 3; }
    function epsilon (uint memory) { return 3; }
    function zeta (uint memory, uint storage) { return 4; }
    function eta (uint memory, uint storage, uint calldata) { return 5; }
}
contract three {
    function theta (int) { return 6; }
    function theta (int calldata, uint storage) { return 7; }
    function iota (uint storage foo) { return 8; }
    function kappa (uint storage foo, uint memory bar) { return 9; }
}
contract four {
    function lambda () returns () { return; }
    function mu () returns (int)  { return 10; }
    function nu () returns (int, int)  { return 11; }
    function xi () internal returns (int, int)  { return 12; }
    function omicron () internal pure returns (int, int)  { return 13; }
}
