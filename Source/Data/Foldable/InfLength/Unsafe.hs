{-|
    This module is just a generalization of the "Data.List.InfLength.Unsafe" module. If you want to read more documentation, go there.
-}

module Data.Foldable.InfLength.Unsafe (
    infLength,
    
    Nat(Zero, Succ),
    intToNat,
    integralToNat,
    natToInt
) where

import Data.Foldable (Foldable, foldl')
import Data.Nat


-- | Lazily returns the length of potentially infinite Foldable data.
infLength :: Foldable f => f a -> Nat
infLength = foldl' (\a _ -> Succ a) Zero

