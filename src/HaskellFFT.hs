module HaskellFFT (dftNaive, fft) where

import Data.Complex

dftNaiveSum :: RealFloat a => Int -> Int -> Int -> [Complex a] -> Complex a
dftNaiveSum k n len [] = 0 :+ 0
dftNaiveSum kInt nInt lenInt (x:xs) = do
    let k = fromIntegral kInt
    let n = fromIntegral nInt
    let len = fromIntegral lenInt
    let v1 = x * (exp (0 :+ (2 * pi * k * n / len)))
    let v2 = (dftNaiveSum kInt (nInt + 1) lenInt xs)
    v1 + v2

dftNaiveIter :: RealFloat a => Int -> [Complex a] -> [Complex a]
dftNaiveIter 0 xs = []
dftNaiveIter n xs = (dftNaiveSum ((length xs) - n) 0 (length xs) xs) : (dftNaiveIter (n - 1) xs)

dftNaive :: RealFloat a => [Complex a] -> [Complex a]
dftNaive xs = dftNaiveIter (length xs) xs

fft :: Int -> Int
fft a = a + 1
