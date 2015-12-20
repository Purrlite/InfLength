{-# LANGUAGE CPP #-}

module Data.Foldable.InfLength.Unsafe (
    Nat(Zero,Succ),
    intToNat,
    integralToNat,
    natToInt,
    infLength
) where

import Prelude hiding (foldl', length)
#if __GLASGOW_HASKELL__ >= 710
import Data.Foldable (foldl', length)
#else
import Data.Foldable (foldl')
length :: (Foldable f) => f a -> Int
length = foldl' (\a _ -> a+1) 0
#endif


data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)


intToNat :: Integer -> Nat
intToNat n
    | n >= 0    = intToNat' n
    | otherwise = error "Negative number being converted to Nat!"
    where intToNat' 0 = Zero
          intToNat' n = Succ $ intToNat' (n-1)


integralToNat :: (Integral i) => i -> Nat
integralToNat = intToNat . toInteger


natToInt :: Nat -> Integer
natToInt Zero     = 0
natToInt (Succ n) = 1 + natToInt n


infLength :: Foldable f => f a -> Nat
infLength = foldl' (\a _ -> Succ a) Zero

