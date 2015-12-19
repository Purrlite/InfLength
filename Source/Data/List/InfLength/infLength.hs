module Data.List.InfLength (
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

import Data.List.InfLength.Unsafe


-- ~ is supposed to be a symbol of infinity
(>~) :: (Integral i) => i -> [a] -> Bool
n >~ list = integralToNat n > infLength list

-- ! is a symbol of a finite list
(!>~) :: [a] -> [b] -> Bool
finList !>~ infList = length finList >~ infList

(~>) :: (Integral i) => [a] -> i -> Bool
list ~> n = infLength list > integralToNat n

(~>!) :: [a] -> [b] -> Bool
infList ~>! finList = infList ~> length finList


(>=~) :: (Integral i) => i -> [a] -> Bool
n >=~ list = integralToNat n >= infLength list

(!>=~) :: [a] -> [b] -> Bool
finList !>=~ infList = length finList >=~ infList

(~>=) :: (Integral i) => [a] -> i -> Bool
list ~>= n = infLength list >= integralToNat n

(~>=!) :: [a] -> [b] -> Bool
infList ~>=! finList = infList ~>= length finList


(<~) :: (Integral i) => i -> [a] -> Bool
n <~ list = integralToNat n < infLength list

(!<~) :: [a] -> [b] -> Bool
finList !<~ infList = length finList <~ infList

(~<) :: (Integral i) => [a] -> i -> Bool
list ~< n = infLength list < integralToNat n

(~<!) :: [a] -> [b] -> Bool
infList ~<! finList = infList ~< length finList


(<=~) :: (Integral i) => i -> [a] -> Bool
n <=~ list = integralToNat n <= infLength list

(!<~) :: [a] -> [b] -> Bool
finList !<=~ infList = length finList <=~ infList

(~<=) :: (Integral i) => [a] -> i -> Bool
list ~<= n = infLength list <= integralToNat n

(~<=!) :: [a] -> [b] -> Bool
infList ~<=! finList = infList ~<= length finList



(==~) :: (Integral i) => i -> [a] -> Bool
n ==~ list = integralToNat n == infLength list

(!==~) :: [a] -> [b] -> Bool
finList !==~ infList = length finList ==~ infList

(~==) :: (Integral i) => [a] -> i -> Bool
list ~== n = infLength list == integralToNat n

(~==!) :: [a] -> [b] -> Bool
infList ~==! finList = infList ~== length finList


(/=~) :: (Integral i) => i -> [a] -> Bool
n /=~ list = integralToNat n /= infLength list

(!/=~) :: [a] -> [b] -> Bool
finList !/=~ infList = length finList /=~ infList

(~/=) :: (Integral i) => [a] -> i -> Bool
list ~/= n = infLength list /= integralToNat n

(~/=!) :: [a] -> [b] -> Bool
infList ~/=! finList = infList ~/= length finList

