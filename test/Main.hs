module Main (main) where

import Data.Complex
import Test.HUnit
import HaskellFFT.FFT
import HaskellFFT.Util

testAllClose1 :: Test
testAllClose1 = TestCase $ do
    assertBool "Vals should be all close" $ allClose [10, 3] [10, 3] 0.1

testAllClose2 :: Test
testAllClose2 = TestCase $ do
    assertBool "Vals should not be all close" $ not $ allClose [10, 3] [10, 3.2] 0.1

testAllClose3 :: Test
testAllClose3 = TestCase $ do
    assertBool "Vals of different lengths should not be all close" $ not $ allClose [10, 3] [10] 0.1

testCreateLinspace1 :: Test
testCreateLinspace1 = TestCase $ do
    let x = createLinspace 4 0 2
    assertBool "Linspace is incorrect" $ allClose x [0, 0.5, 1, 1.5] 0.01

testGetElem1 :: Test
testGetElem1 = TestCase $ do
    let x = [5, 3, 7, 2]
    assertEqual "getElem gets element at index" (getElem x 2) (Just 7)

testGetElem2 :: Test
testGetElem2 = TestCase $ do
    let x = [5, 3, 7, 2]
    assertEqual "getElem gets element at index" (getElem x 0) (Just 5)

testGetElem3 :: Test
testGetElem3 = TestCase $ do
    let x = [5, 3, 7, 2]
    assertEqual "getElem gets element at index" (getElem x 4) (Nothing)

testSetElem1 :: Test
testSetElem1 = TestCase $ do
    let x = [5, 3, 7, 2]
    let new = setElem x 2 4
    let expected = [5, 3, 4, 2]
    assertEqual "setElem sets new value in list" new expected

testSetElem2 :: Test
testSetElem2 = TestCase $ do
    let x = [5, 3, 7, 2]
    let new = setElem x 0 1
    let expected = [1, 3, 7, 2]
    assertEqual "setElem sets new value in list" new expected

testDftNaive1 :: Test
testDftNaive1 = TestCase $ do
    let elemCount = 100
    let halfElemCount = 0.5 * fromIntegral elemCount
    let linspace = createLinspace elemCount 0 (2 * pi)
    let imagCos = map (\x -> (cos(x) :+ 0)) linspace
    let dftRes = dftNaive imagCos
    let realRes = map realPart dftRes
    let zeros = replicate elemCount 0
    let expected = setElem (setElem zeros (elemCount - 1) halfElemCount) 1 halfElemCount
    assertBool "dft naive cos fails" $ allClose realRes expected 0.001

testDftNaive2 :: Test
testDftNaive2 = TestCase $ do
    let elemCount = 100
    let halfElemCount = 0.5 * fromIntegral elemCount
    let linspace = createLinspace elemCount 0 (2 * pi)
    let imagCos = map (\x -> (cos(3 * x) :+ 0)) linspace
    let dftRes = dftNaive imagCos
    let realRes = map realPart dftRes
    let zeros = replicate elemCount 0
    let expected = setElem (setElem zeros (elemCount - 3) halfElemCount) 3 halfElemCount
    assertBool "dft naive cos fails" $ allClose realRes expected 0.001

testDftIdftNaive1 :: Test
testDftIdftNaive1 = TestCase $ do
    let elemCount = 100
    let zeros = replicate elemCount 0
    let realInput = setElem zeros 2 1
    let input = map (\x -> x :+ 0) realInput
    let dftRes = dftNaive input
    let idftRes = idftNaive dftRes
    let realRes = map realPart idftRes
    assertBool "dft followed by idft is correct" $ allClose realRes realInput 0.001

testDistFromPow2 :: Test
testDistFromPow2 = TestCase $ do
    let n = 53
    let dist = distFromPow2 n
    let expected = 64 - n
    assertEqual "dist from pow2" dist expected

testPadPow2 :: Test
testPadPow2 = TestCase $ do
    let vals = [1, 2, 3, 4, 5]
    let padded = padPow2 vals 0
    let expected = [1, 2, 3, 4, 5, 0, 0, 0]
    assertEqual "padded correctly" padded expected

testDftIdftNaivePadded :: Test
testDftIdftNaivePadded = TestCase $ do
    let elemCount = 50
    let halfElemCount = 0.5 * fromIntegral elemCount
    let linspace = createLinspace elemCount 0 (2 * pi)
    let imagCos = map (\x -> (cos(x) :+ 0)) linspace
    let padded = padPow2 imagCos (0 :+ 0)
    let dftRes = dftNaive padded
    let realDftRes = map realPart dftRes
    let idftRes = idftNaive dftRes
    let realIdftRes = map realPart idftRes
    let expected = map realPart padded
    -- putStrLn $ show realIdftRes
    assertBool "dft idft naive" $ allClose realIdftRes expected 0.001

main :: IO Counts
main = do
    -- putStrLn $ show $ dftNaive [0 :+ 0, 1 :+ 0, 0 :+ 0]
    -- let linspace = createLinspace 10 0 (2 * pi)
    -- let imag = map (\x -> (cos(3 * x) :+ 0)) linspace
    let imag = [0, 0, 1, 0]
    let res1 = dftNaive imag
    let res2 = idftNaive res1
    -- putStrLn $ show res2
    runTestTT $ TestList [testAllClose1,
                          testAllClose2,
                          testAllClose3,
                          testCreateLinspace1,
                          testGetElem1,
                          testGetElem2,
                          testGetElem3,
                          testSetElem1,
                          testSetElem2,
                          testDftNaive1,
                          testDftNaive2,
                          testDftIdftNaive1,
                          testDistFromPow2,
                          testPadPow2,
                          testDftIdftNaivePadded]
