module Main (main) where

import Data.Complex
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

-- createLinspace :: RealFloat a => Int -> Int -> a -> a -> [a]
-- createLinspace 0 _ _ _ = []
-- createLinspace n tot a b = 

-- createComplexSine :: Int -> Int -> [Complex a]
-- createComplexSine 0 tot = []
-- createComplexSine n tot = do
--     let x = 2 * pi * (fromIntegral (tot - n)) / (fromIntegral (tot))
--     let val = sin(x)
--     let complex = ((val) :+ 0)
--     complex : createComplexSineIter (n - 1) tot

main :: IO Counts
main = do
    putStrLn $ show $ dftNaive [0 :+ 0, 1 :+ 0, 0 :+ 0]
    runTestTT $ TestList [testAllClose1, testAllClose2, testAllClose3]
