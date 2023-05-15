import PRF
import GHC.Natural (Natural)
import Test.QuickCheck
    ( stdArgs
    , quickCheckWith
    , Args
    , maxSuccess
    )
import Test.QuickCheck.Instances.Natural ()

prop_addN :: Natural -> Natural -> Bool
prop_addN m n = natToNatural (addNat m' n') == m + n
    where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n

prop_idN :: Natural -> Bool
prop_idN m = natToNatural (idNat m') == m
    where
    m':: Nat
    m' = naturalToNat m

prop_subNat :: Natural -> Natural -> Bool
prop_subNat m n =
    if n > m then natToNatural (subNat m' n') == 0
    else natToNatural (subNat m' n') == m - n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

prop_multN :: Natural -> Natural -> Bool
prop_multN m n = natToNatural (multNat m' n') == m * n
    where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n

args :: Args
args = stdArgs { maxSuccess = 1000 }

-- Run QuickCheck tests
main :: IO ()
main = do
    quickCheck prop_addN
    quickCheck prop_idN
    quickCheck prop_multN
    quickCheckWith args prop_subNat