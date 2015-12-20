module Data.Foldable.InfLength (
    (>~),
    (!>~),
    (~>),
    (~>!),
    
    (>=~),
    (!>=~),
    (~>=),
    (~>=!),
    
    (<~),
    (!<~),
    (~<),
    (~<!),
    
    (<=~),
    (!<=~),
    (~<=),
    (~<=!),
    
    (==~),
    (!==~),
    (~==),
    (~==!),
    
    (/=~),
    (!/=~),
    (~/=),
    (~/=!)
) where

import Prelude hiding (length, foldl')
#if __GLASGOW_HASKELL__ >= 710
import Data.Foldable (Foldable, length)
#else
import Data.Foldable (Foldable, foldl')
length :: (Foldable f) => f a -> Int
length = foldl' (\a _ -> a+1) 0
#endif
import Data.Foldable.InfLength.Unsafe


(>~) :: (Foldable f, Integral i) => i -> f a -> Bool
n >~ list = integralToNat n > infLength list

(!>~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !>~ infList = length finList >~ infList

(~>) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~> n = infLength list > integralToNat n

(~>!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~>! finList = infList ~> length finList


(>=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n >=~ list = integralToNat n >= infLength list

(!>=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !>=~ infList = length finList >=~ infList

(~>=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~>= n = infLength list >= integralToNat n

(~>=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~>=! finList = infList ~>= length finList


(<~) :: (Foldable f, Integral i) => i -> f a -> Bool
n <~ list = integralToNat n < infLength list

(!<~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !<~ infList = length finList <~ infList

(~<) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~< n = infLength list < integralToNat n

(~<!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~<! finList = infList ~< length finList


(<=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n <=~ list = integralToNat n <= infLength list

(!<=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !<=~ infList = length finList <=~ infList

(~<=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~<= n = infLength list <= integralToNat n

(~<=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~<=! finList = infList ~<= length finList



(==~) :: (Foldable f, Integral i) => i -> f a -> Bool
n ==~ list = integralToNat n == infLength list

(!==~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !==~ infList = length finList ==~ infList

(~==) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~== n = infLength list == integralToNat n

(~==!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~==! finList = infList ~== length finList


(/=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n /=~ list = integralToNat n /= infLength list

(!/=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !/=~ infList = length finList /=~ infList

(~/=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~/= n = infLength list /= integralToNat n

(~/=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~/=! finList = infList ~/= length finList

