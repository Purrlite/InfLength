module Data.Foldable.InfLength.Unsafe (
    Nat(Zero,Succ),
    intToNat,
    integralToNat,
    natToInt,
    infLength
) where

import Prelude hiding (foldl')
import Data.Foldable (Foldable, foldl')


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

