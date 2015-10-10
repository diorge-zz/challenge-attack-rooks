# Attack Rooks
This is a solution to the Attack Rooks problem proposed by ACM-ICPC.
It is a variant of the N-Queens problem, but with Rooks and blocking pawns.

## Problem description
Given a NxN (N > 0) chess board containing some pawns, output the highest
possible number of rooks that can be put in this board without any rook being
able to capture each other.

## Input format
The input format should be a string containing only '.', '|' and 'X'
(uppercase).

  * '|' is used to separate rows
  * '.' is used for empty spaces
  * 'X' is used for pawns.

Also, each row must have the same amount of entries, and the number of
columns and rows must be the same.
