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
