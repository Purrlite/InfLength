module Data.List.InfLength.Unsafe (
    Nat(Zero,Succ),
    intToNat,
    integralToNat,
    natToInt,
    infLength    
) where


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


infLength :: [a] -> Nat
infLength []     = Zero
infLength (_:xs) = Succ $ infLength xs
