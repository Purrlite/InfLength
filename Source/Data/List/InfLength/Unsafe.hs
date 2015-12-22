module Data.List.InfLength.Unsafe (
    Nat(Zero,Succ),
    intToNat,
    integralToNat,
    natToInt,
    infLength
) where

import Data.Nat


infLength :: [a] -> Nat
infLength []     = Zero
infLength (_:xs) = Succ $ infLength xs

