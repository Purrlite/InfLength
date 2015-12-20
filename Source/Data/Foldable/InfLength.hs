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

import Prelude hiding (length)
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

