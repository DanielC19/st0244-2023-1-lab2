module PRF where
import GHC.Natural (Natural)

-- Definition of the Nat datatype
data Nat = Zero | Succ Nat

-- Func to cast Nat into Natural (normal numbers)
natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

-- Func to cast Natural into Nat (with Succ)
naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n - 1))

-- Definition of recNat to use in the definition of all PRF
recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

-- Identity function
-- returns same value that enters
idNat :: Nat -> Nat
idNat m =
    let idNatHelper :: Nat -> Nat -> Nat
        idNatHelper _ x = x
    in recNat m idNatHelper m

-- Predecessor function
-- returns the entered value minus 1
predNat :: Nat -> Nat
predNat m =
    let predNatHelper :: Nat -> Nat -> Nat
        predNatHelper _ Zero = Zero
        predNatHelper _ (Succ x) = x
    in recNat m predNatHelper (Succ Zero)

-- Add functions
-- returns the add of two Nat
addNat :: Nat -> Nat -> Nat
addNat m n =
    let addNatHelper :: Nat -> Nat -> Nat
        addNatHelper _ x = Succ x
    in recNat n addNatHelper m

-- Subtraction function
-- returns the subtraction of two Nat
-- if the second parameter is greater than the first one, returns Zero
subNat :: Nat -> Nat -> Nat
subNat m n =
    let subNatHelper :: Nat -> Nat -> Nat
        subNatHelper _ Zero = Zero
        subNatHelper _ (Succ x) = x
    in recNat m subNatHelper n

-- Multiplication function
-- return the multiplication of two Nat
multNat :: Nat -> Nat -> Nat
multNat m n =
    let multNatHelper :: Nat -> Nat -> Nat
        multNatHelper _ x = addNat m x
    in recNat Zero multNatHelper n
