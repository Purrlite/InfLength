{-|
    This module exposes function infLength, on which the whole infinite length checking is built upon, and Nats, which are also necessary for it to work.
    
    As the name of the module implies, the function isn't very safe and can easily cause infinite loops if used incorrectly. Some of the reasons that might cause it to loop infinitely include:
    
      * Trying to convert Nat to something not as lazy as Nat
      
      * Trying to fully evaluate the returned Nat, when the list is infinite
-}
module Data.List.InfLength.Unsafe (
    infLength,
    
    Nat(Zero, Succ),
    intToNat,
    integralToNat,
    natToInt
) where

import Data.Nat

-- | Lazily returns the length of a (potentially infinite) list as Nat.
-- Be careful with how you use it as it can easily cause infinite loop if the list is infinite and you use it badly.
-- 
-- Note that this function returns Nats instead of a more general type on purpose. Converting Nats to a number when the list is infinitely long would cause an infinite loop.
infLength :: [a] -> Nat
infLength []     = Zero
infLength (_:xs) = Succ $ infLength xs

