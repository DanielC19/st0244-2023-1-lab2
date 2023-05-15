import PRF
import GHC.Natural (Natural)
import Test.QuickCheck
  ( Arbitrary ( arbitrary, shrink)
  , Property
  , arbitrarySizedNatural
  , quickCheck
  , stdArgs
  , quickCheckWith
  , Args
  , maxSuccess
  , (==>)
  , shrinkIntegral
  )

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

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

prop_subNat :: Natural -> Natural -> Property
prop_subNat m n = n <= m ==> natToNatural (subNat m' n') == m - n
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