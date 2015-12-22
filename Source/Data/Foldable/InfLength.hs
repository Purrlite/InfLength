{-# LANGUAGE CPP #-}

{-|
    This module is just a generalization of the "Data.List.InfLength" module. If you want to read more documentation, go there.
-}

module Data.Foldable.InfLength (
-- * 1.a) Length checking - right
    (>~),
    (<~),
    (>=~),
    (<=~),
    (==~),
    (/=~),
    
-- * 1.b) Length checking - left
    (~>),
    (~<),
    (~>=),
    (~<=),
    (~==),
    (~/=),
    
-- * 2.a) Comparisons of lengths - right
    (!>~),
    (!<~),
    (!>=~),
    (!<=~),
    (!==~),
    (!/=~),
    
-- * 2.b) Comparisons of lengths - left
    (~>!),
    (~<!),
    (~>=!),
    (~<=!),
    (~==!),
    (~/=!)
) where

import Prelude hiding (length)
import Data.Foldable.InfLength.Unsafe
#if __GLASGOW_HASKELL__ >= 710
import Data.Foldable (Foldable, length)
#else
import Data.Foldable (Foldable, foldl')
length :: (Foldable f) => f a -> Int
length = foldl' (\a _ -> a+1) 0
#endif


-- ######################################
-- #### 1.a) Length checking - right ####
-- ######################################

-- | Checks if the number is bigger than the size of the Foldable data
(>~) :: (Foldable f, Integral i) => i -> f a -> Bool
n >~ list = integralToNat n > infLength list

-- | Checks if the number is smaller than the size of the Foldable data
(<~) :: (Foldable f, Integral i) => i -> f a -> Bool
n <~ list = integralToNat n < infLength list

-- | Checks if the number is bigger or equal to the size of the Foldable data
(>=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n >=~ list = integralToNat n >= infLength list

-- | Checks if the number is smaller or equal to the size of the Foldable data
(<=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n <=~ list = integralToNat n <= infLength list

-- | Checks if the number is equal to the size of the Foldable data
(==~) :: (Foldable f, Integral i) => i -> f a -> Bool
n ==~ list = integralToNat n == infLength list

-- | Checks if the number is not equal to the size of the Foldable data
(/=~) :: (Foldable f, Integral i) => i -> f a -> Bool
n /=~ list = integralToNat n /= infLength list


-- #####################################
-- #### 1.b) Length checking - left ####
-- #####################################

-- | Checks if the size of the Foldable data is bigger than the number
(~>) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~> n = infLength list > integralToNat n

-- | Checks if the size of the Foldable data is smaller than the number
(~<) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~< n = infLength list < integralToNat n

-- | Checks if the size of the Foldable data is bigger or equal to the number
(~>=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~>= n = infLength list >= integralToNat n

-- | Checks if the size of the Foldable data is smaller or equal to the number
(~<=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~<= n = infLength list <= integralToNat n

-- | Checks if the size of the Foldable data is equal to the number
(~==) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~== n = infLength list == integralToNat n

-- | Checks if the size of the Foldable data is not equal to the number
(~/=) :: (Foldable f, Integral i) => f a -> i -> Bool
list ~/= n = infLength list /= integralToNat n


-- #############################################
-- #### 2.b) Comparisons of lengths - right ####
-- #############################################

-- | Checks if the size of the finite Foldable data is bigger than the length of the (potentially infinite) Foldable data
(!>~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !>~ infList = length finList >~ infList

-- | Checks if the size of the finite Foldable data is smaller than the length of the (potentially infinite) Foldable data
(!<~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !<~ infList = length finList <~ infList

-- | Checks if the size of the finite Foldable data is bigger or equal to the length of the (potentially infinite) Foldable data
(!>=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !>=~ infList = length finList >=~ infList

-- | Checks if the size of the finite Foldable data is smaller or equal to the length of the (potentially infinite) Foldable data
(!<=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !<=~ infList = length finList <=~ infList

-- | Checks if the size of the finite Foldable data is equal to the length of the (potentially infinite) Foldable data
(!==~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !==~ infList = length finList ==~ infList

-- | Checks if the size of the finite Foldable data is not equal to the length of the (potentially infinite) Foldable data
(!/=~) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
finList !/=~ infList = length finList /=~ infList


-- ############################################
-- #### 2.b) Comparisons of lengths - left ####
-- ############################################

-- | Checks if the size of the (potentially infinite) Foldable data is bigger than the size of the finite Foldable data
(~>!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~>! finList = infList ~> length finList

-- | Checks if the size of the (potentially infinite) Foldable data is smaller than the size of the finite Foldable data
(~<!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~<! finList = infList ~< length finList

-- | Checks if the size of the (potentially infinite) Foldable data is bigger or equal to the size of the finite Foldable data
(~>=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~>=! finList = infList ~>= length finList

-- | Checks if the size of the (potentially infinite) Foldable data is smaller or equal to the size of the finite Foldable data
(~<=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~<=! finList = infList ~<= length finList

-- | Checks if the size of the (potentially infinite) Foldable data is equal to the size of the finite Foldable data
(~==!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~==! finList = infList ~== length finList

-- | Checks if the size of the (potentially infinite) Foldable data is not equal to the size of the finite Foldable data
(~/=!) :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Bool
infList ~/=! finList = infList ~/= length finList

