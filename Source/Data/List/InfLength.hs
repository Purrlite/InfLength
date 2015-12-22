{-|
    This module exposes 2 sets of functions.
    
      (1) Ord and Eq functions with @~@ added to one of the sides of the operator
      
      (2) Ord and Eq functions with @~@ added to one side and @!@ to the other side of the operator
      
    The first set of functions compares the length of a (potentially infinite) list to an Integral number. The list is expected on the side of the operator, where you can find the @~@ symbol at.
    
    The second set of functions compares the length of a (potentially infinite) list to the length of a finite list. The potentially infinite list is expected on the side with @~@ and the finite list on the side with @!@. If you try calling this set of functions with an infinite list as an argument on the @!@ side, then the function __will not__ terminate.
    
    The @~@ symbol was chosen, because it's the closest symbol one can find on normal keyboards that is close in appearance to the symbol of infinity. So hopefully that helps with remembering it. 
-}


module Data.List.InfLength (
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

import Data.List.InfLength.Unsafe


-- ######################################
-- #### 1.a) Length checking - right ####
-- ######################################

-- | Checks if the number is bigger than the length of the list
(>~) :: (Integral i) => i -> [a] -> Bool
n >~ list = integralToNat n > infLength list

-- | Checks if the number is smaller than the length of the list
(<~) :: (Integral i) => i -> [a] -> Bool
n <~ list = integralToNat n < infLength list

-- | Checks if the number is bigger or equal to the length of the list
(>=~) :: (Integral i) => i -> [a] -> Bool
n >=~ list = integralToNat n >= infLength list

-- | Checks if the number is smaller or equal to the length of the list
(<=~) :: (Integral i) => i -> [a] -> Bool
n <=~ list = integralToNat n <= infLength list

-- | Checks if the number is equal to the length of the list
(==~) :: (Integral i) => i -> [a] -> Bool
n ==~ list = integralToNat n == infLength list

-- | Checks if the number is not equal to the length of the list
(/=~) :: (Integral i) => i -> [a] -> Bool
n /=~ list = integralToNat n /= infLength list


-- #####################################
-- #### 1.b) Length checking - left ####
-- #####################################

-- | Checks if the length of the list is bigger than the number
(~>) :: (Integral i) => [a] -> i -> Bool
list ~> n = infLength list > integralToNat n

-- | Checks if the length of the list is smaller than the number
(~<) :: (Integral i) => [a] -> i -> Bool
list ~< n = infLength list < integralToNat n

-- | Checks if the length of the list is bigger or equal to the number
(~>=) :: (Integral i) => [a] -> i -> Bool
list ~>= n = infLength list >= integralToNat n

-- | Checks if the length of the list is smaller or equal to the number
(~<=) :: (Integral i) => [a] -> i -> Bool
list ~<= n = infLength list <= integralToNat n

-- | Checks if the length of the list is equal to the number
(~==) :: (Integral i) => [a] -> i -> Bool
list ~== n = infLength list == integralToNat n

-- | Checks if the length of the list is not equal to the number
(~/=) :: (Integral i) => [a] -> i -> Bool
list ~/= n = infLength list /= integralToNat n


-- #############################################
-- #### 2.a) Comparisons of lengths - right ####
-- #############################################

-- | Checks if the length of the finite list is bigger than the length of the (potentially infinite) list
(!>~) :: [a] -> [b] -> Bool
finList !>~ infList = length finList >~ infList

-- | Checks if the length of the finite list is smaller than the length of the (potentially infinite) list
(!<~) :: [a] -> [b] -> Bool
finList !<~ infList = length finList <~ infList

-- | Checks if the length of the finite list is bigger or equal to the length of the (potentially infinite) list
(!>=~) :: [a] -> [b] -> Bool
finList !>=~ infList = length finList >=~ infList

-- | Checks if the length of the finite list is smaller or equal to the length of the (potentially infinite) list
(!<=~) :: [a] -> [b] -> Bool
finList !<=~ infList = length finList <=~ infList

-- | Checks if the length of the finite list is equal to the length of the (potentially infinite) list
(!==~) :: [a] -> [b] -> Bool
finList !==~ infList = length finList ==~ infList

-- | Checks if the length of the finite list is not equal to the length of the (potentially infinite) list
(!/=~) :: [a] -> [b] -> Bool
finList !/=~ infList = length finList /=~ infList


-- ############################################
-- #### 2.b) Comparisons of lengths - left ####
-- ############################################

-- | Checks if the length of the (potentially infinite) list is bigger than the length of the finite list
(~>!) :: [a] -> [b] -> Bool
infList ~>! finList = infList ~> length finList

-- | Checks if the length of the (potentially infinite) list is smaller than the length of the finite list
(~<!) :: [a] -> [b] -> Bool
infList ~<! finList = infList ~< length finList

-- | Checks if the length of the (potentially infinite) list is bigger or equal to the length of the finite list
(~>=!) :: [a] -> [b] -> Bool
infList ~>=! finList = infList ~>= length finList

-- | Checks if the length of the (potentially infinite) list is smaller or equal to the length of the finite list
(~<=!) :: [a] -> [b] -> Bool
infList ~<=! finList = infList ~<= length finList

-- | Checks if the length of the (potentially infinite) list is equal to the length of the finite list
(~==!) :: [a] -> [b] -> Bool
infList ~==! finList = infList ~== length finList

-- | Checks if the length of the (potentially infinite) list is equal to the length of the finite list
(~/=!) :: [a] -> [b] -> Bool
infList ~/=! finList = infList ~/= length finList

