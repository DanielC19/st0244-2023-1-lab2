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

prop_predN :: Natural -> Bool
prop_predN m =
    if m == 0 then natToNatural (predNat m') == 0
    else natToNatural (predNat m') == m-1
    where
    m':: Nat
    m' = naturalToNat m

args :: Args
args = stdArgs { maxSuccess = 10000 }

-- Run QuickCheck tests
main :: IO ()
main = do
    quickCheckWith args prop_idN
    quickCheckWith args prop_subNat
    quickCheckWith args prop_addN
    quickCheckWith args prop_multN
    quickCheckWith args prop_predN