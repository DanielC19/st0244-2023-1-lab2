module PRF where
import GHC.Natural (Natural)

data Nat = Zero | Succ Nat

natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n - 1))

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

idNat :: Nat -> Nat
idNat m =
    let idNatHelper :: Nat -> Nat -> Nat
        idNatHelper _ x = x
    in recNat m idNatHelper m

predNat :: Nat -> Nat
predNat m =
    let predNatHelper :: Nat -> Nat -> Nat
        predNatHelper _ Zero = Zero
        predNatHelper _ (Succ x) = x
    in recNat m predNatHelper (Succ Zero)

addNat :: Nat -> Nat -> Nat
addNat m n =
    let addNatHelper :: Nat -> Nat -> Nat
        addNatHelper _ x = Succ x
    in recNat n addNatHelper m

subNat :: Nat -> Nat -> Nat
subNat m n =
    let subNatHelper :: Nat -> Nat -> Nat
        subNatHelper _ Zero = Zero
        subNatHelper _ (Succ x) = x
    in recNat m subNatHelper n

multNat :: Nat -> Nat -> Nat
multNat m n =
    let multNatHelper :: Nat -> Nat -> Nat
        multNatHelper _ x = addNat m x
    in recNat Zero multNatHelper n
