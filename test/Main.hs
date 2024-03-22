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

createLinspaceIter :: RealFloat a => Int -> Int -> a -> a -> [a]
createLinspaceIter 0 _ _ _ = []
createLinspaceIter n tot a b = do
    let x = a + (b - a) * (fromIntegral (tot - n)) / (fromIntegral (tot))
    x : createLinspaceIter (n - 1) tot a b

createLinspace :: RealFloat a => Int -> a -> a -> [a]
createLinspace n = createLinspaceIter n n

testCreateLinspace1 :: Test
testCreateLinspace1 = TestCase $ do
    let x = createLinspace 4 0 2
    assertBool "Linspace is incorrect" $ allClose x [0, 0.5, 1, 1.5] 0.01

main :: IO Counts
main = do
    -- putStrLn $ show $ dftNaive [0 :+ 0, 1 :+ 0, 0 :+ 0]
    let x = createLinspace 5 0 (2 * pi)
    putStrLn $ show x
    runTestTT $ TestList [testAllClose1, testAllClose2, testAllClose3, testCreateLinspace1]
