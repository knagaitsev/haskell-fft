module Main (main) where

import Test.HUnit

import HaskellFFT

allClose :: (Ord a, Num a) => [a] -> [a] -> a -> Bool
allClose [] [] _ = True
allClose (a:as) (b:bs) threshold = ((abs $ a - b) <= threshold) && (allClose as bs threshold)
allClose _ _ _ = False

testAllClose1 :: Test
testAllClose1 = TestCase $ do
    assertBool "Vals should be all close" $ allClose [10, 3] [10, 3] 0.1

testAllClose2 :: Test
testAllClose2 = TestCase $ do
    assertBool "Vals should not be all close" $ not $ allClose [10, 3] [10, 3.2] 0.1

testAllClose3 :: Test
testAllClose3 = TestCase $ do
    assertBool "Vals of different lengths should not be all close" $ not $ allClose [10, 3] [10] 0.1

main :: IO Counts
main = runTestTT $ TestList [testAllClose1, testAllClose2, testAllClose3]
