module Main (main) where

import Test.HUnit

import HaskellFFT

testAddition :: Test
testAddition = TestCase $ do
    assertEqual "1 + 1 should be 2" 2 (1 + 1)

testSubtraction :: Test
testSubtraction = TestCase $ do
    assertEqual "1 - 1 should be 0" 0 (1 - 1)

main :: IO Counts
main = runTestTT $ TestList [testAddition, testSubtraction]
