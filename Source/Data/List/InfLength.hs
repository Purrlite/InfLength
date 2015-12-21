module Data.List.InfLength (
    (>~),
    (<~),
    (>=~),
    (<=~),
    (==~),
    (/=~),
    
    (~>),
    (~<),
    (~>=),
    (~<=),
    (~==),
    (~/=),
    
    (!>~),
    (!<~),
    (!>=~),
    (!<=~),
    (!==~),
    (!/=~),
    
    (~>!),
    (~<!),
    (~>=!),
    (~<=!),
    (~==!),
    (~/=!)
) where

import Data.List.InfLength.Unsafe


(>~) :: (Integral i) => i -> [a] -> Bool
n >~ list = integralToNat n > infLength list

(<~) :: (Integral i) => i -> [a] -> Bool
n <~ list = integralToNat n < infLength list

(>=~) :: (Integral i) => i -> [a] -> Bool
n >=~ list = integralToNat n >= infLength list

(<=~) :: (Integral i) => i -> [a] -> Bool
n <=~ list = integralToNat n <= infLength list

(==~) :: (Integral i) => i -> [a] -> Bool
n ==~ list = integralToNat n == infLength list

(/=~) :: (Integral i) => i -> [a] -> Bool
n /=~ list = integralToNat n /= infLength list


(~>) :: (Integral i) => [a] -> i -> Bool
list ~> n = infLength list > integralToNat n

(~<) :: (Integral i) => [a] -> i -> Bool
list ~< n = infLength list < integralToNat n

(~>=) :: (Integral i) => [a] -> i -> Bool
list ~>= n = infLength list >= integralToNat n

(~<=) :: (Integral i) => [a] -> i -> Bool
list ~<= n = infLength list <= integralToNat n

(~==) :: (Integral i) => [a] -> i -> Bool
list ~== n = infLength list == integralToNat n

(~/=) :: (Integral i) => [a] -> i -> Bool
list ~/= n = infLength list /= integralToNat n



(!>~) :: [a] -> [b] -> Bool
finList !>~ infList = length finList >~ infList

(!<~) :: [a] -> [b] -> Bool
finList !<~ infList = length finList <~ infList

(!>=~) :: [a] -> [b] -> Bool
finList !>=~ infList = length finList >=~ infList

(!<=~) :: [a] -> [b] -> Bool
finList !<=~ infList = length finList <=~ infList

(!==~) :: [a] -> [b] -> Bool
finList !==~ infList = length finList ==~ infList

(!/=~) :: [a] -> [b] -> Bool
finList !/=~ infList = length finList /=~ infList


(~>!) :: [a] -> [b] -> Bool
infList ~>! finList = infList ~> length finList

(~<!) :: [a] -> [b] -> Bool
infList ~<! finList = infList ~< length finList

(~>=!) :: [a] -> [b] -> Bool
infList ~>=! finList = infList ~>= length finList

(~<=!) :: [a] -> [b] -> Bool
infList ~<=! finList = infList ~<= length finList

(~==!) :: [a] -> [b] -> Bool
infList ~==! finList = infList ~== length finList

(~/=!) :: [a] -> [b] -> Bool
infList ~/=! finList = infList ~/= length finList

