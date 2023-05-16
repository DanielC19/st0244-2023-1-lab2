-- we import the necessary libraries and the PRF file
import PRF
import GHC.Natural (Natural)
import Test.QuickCheck
    ( stdArgs
    , quickCheckWith
    , Args
    , maxSuccess
    )
import Test.QuickCheck.Instances.Natural ()

-- test function of add
-- cast to Natural the result of addNat
-- and compare it to the actual add of Naturals
prop_addN :: Natural -> Natural -> Bool
prop_addN m n = natToNatural (addNat m' n') == m + n
    where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n

-- test function of identity
-- cast the value of idNat to Naturals
-- and check that it returns the same value
prop_idN :: Natural -> Bool
prop_idN m = natToNatural (idNat m') == m
    where
    m':: Nat
    m' = naturalToNat m

-- test function of subtraction
-- cast to Natural the result of subNat
-- and compare it to the actual subtraction of Naturals
prop_subNat :: Natural -> Natural -> Bool
prop_subNat m n =
    if n > m then natToNatural (subNat m' n') == 0
    else natToNatural (subNat m' n') == m - n
    where
    m', n':: Nat
    m' = naturalToNat m
    n' = naturalToNat n

-- test function of multiplication
-- cast to Natural the result of multNat
-- and compare it to the actual multiplication of Naturals
prop_multN :: Natural -> Natural -> Bool
prop_multN m n = natToNatural (multNat m' n') == m * n
    where
    m', n' :: Nat
    m' = naturalToNat m
    n' = naturalToNat n

-- test function of predecessor
-- cast to Natural the result of predNat
-- and compare it to the value of the number minus one
prop_predN :: Natural -> Bool
prop_predN m =
    if m == 0 then natToNatural (predNat m') == 0
    else natToNatural (predNat m') == m-1
    where
    m':: Nat
    m' = naturalToNat m

-- change args to have more test cases
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