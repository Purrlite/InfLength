module Data.Foldable.InfLength.Unsafe (
    Nat(Zero,Succ),
    intToNat,
    integralToNat,
    natToInt,
    infLength
) where

import Data.Foldable (Foldable, foldl')
import Data.Nat


infLength :: Foldable f => f a -> Nat
infLength = foldl' (\a _ -> Succ a) Zero

