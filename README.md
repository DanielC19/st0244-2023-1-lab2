# st0244-2023-1-lab2

## By:
- Juan Diego Robles de la Ossa
- Daniel Correa Botero

## Used OS for develop:
- Windows 10

## GHC version:
- 9.6.1

## QuickCheck version:
- 2.14.2

## QuickCheck Instances version:
- 0.3.29.1

## Project Overview

First of all, we created the algebraic datatype Nat, which defined Naturals
as Zero and its successors, just as it appeared at the guide. As well as
recNat, a recursive structure to implement upon it 5 Primitive Recursive
Functions, the main goal of the lab.

We implemented the following functions:
- Identity
- Predecessor
- Add
- Subtraction
- Multiplication

Each one was tested using QuickCheck on the versions described before.
For that, we had to create a function to test random values and compare
the actual value to the outputs given by our primitive functions.
We changed the arguments of QuickCheck to validate 10000 cases instead of
the default 100 cases.