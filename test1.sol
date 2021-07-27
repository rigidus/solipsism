pragma solidity ^0.8.0;
pragma solidity ^0.8.1;

contract one {
    function alfa () { return; }
}

contract two {
    function beta (uint) { return 1; }
    function gamma (uint, uint) { return 2; }
    function delta (uint, uint, uint) { return 3; }
}

-----------------------

function delta (uint memory) { return 3; }
function gamma (uint256, string) { return 1; }
function delta (uint256 storage) { return 1; }
function delta (uint256 storage, string) { return 1; }
function epsilon (uint256 storage, string memory) { return 1; }
function epsilon (uint memory, int storage) { return 4; }
function beta (a) { return 1; }
function gamma (a, b) { return 1; }
