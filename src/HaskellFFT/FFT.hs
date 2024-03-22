module HaskellFFT.FFT (dftNaive, idftNaive, fft, ifft) where

import Data.Complex
import HaskellFFT.Util

dftNaiveSum :: RealFloat a => Int -> Int -> Int -> [Complex a] -> Complex a
dftNaiveSum k n len [] = 0 :+ 0
dftNaiveSum kInt nInt lenInt (x:xs) = do
    let k = fromIntegral kInt
    let n = fromIntegral nInt
    let len = fromIntegral lenInt
    let v1 = x * (exp (0 :+ (-2 * pi * k * n / len)))
    let v2 = (dftNaiveSum kInt (nInt + 1) lenInt xs)
    v1 + v2

dftNaiveIter :: RealFloat a => Int -> [Complex a] -> [Complex a]
dftNaiveIter 0 xs = []
dftNaiveIter n xs = (dftNaiveSum ((length xs) - n) 0 (length xs) xs) : (dftNaiveIter (n - 1) xs)

dftNaive :: RealFloat a => [Complex a] -> [Complex a]
dftNaive xs = dftNaiveIter (length xs) xs

idftNaiveSum :: RealFloat a => Int -> Int -> Int -> [Complex a] -> Complex a
idftNaiveSum k n len [] = 0 :+ 0
idftNaiveSum kInt nInt lenInt (x:xs) = do
    let k = fromIntegral kInt
    let n = fromIntegral nInt
    let len = fromIntegral lenInt
    let v1 = x * (exp (0 :+ (2 * pi * k * n / len)))
    let v2 = (idftNaiveSum kInt (nInt + 1) lenInt xs)
    v1 + v2

idftNaiveIter :: RealFloat a => Int -> [Complex a] -> [Complex a]
idftNaiveIter 0 xs = []
idftNaiveIter n xs = do
    let sum = (idftNaiveSum ((length xs) - n) 0 (length xs) xs)
    let adjusted = ((1 / fromIntegral (length xs)) :+ 0) * sum
    adjusted : (idftNaiveIter (n - 1) xs)

idftNaive :: RealFloat a => [Complex a] -> [Complex a]
idftNaive xs = idftNaiveIter (length xs) xs

fftSum :: RealFloat a => [Complex a] -> [Complex a] -> [Complex a] -> [Complex a]
fftSum evenElems factors oddElems = zipWith (+) evenElems $ zipWith (*) factors oddElems

fftIter :: RealFloat a => [Int] -> [Complex a] -> [Complex a]
fftIter ks [] = [] -- bad
fftIter ks [x] = replicate (length ks) x
fftIter ks xs = do
    let evenElems = evenIdxs xs
    let oddElems = oddIdxs xs
    let factors = map (\k -> exp(0 :+ (-2 * pi * (fromIntegral k) / fromIntegral (length xs)))) ks
    let evenRes = fftIter ks evenElems
    let oddRes = fftIter ks oddElems
    fftSum evenRes factors oddRes

-- Currently fft only operates on powers of 2
-- Idea of fft divide-and-conquer: do k operations at each phase for a total
-- of log(n) * k (where k = n)
fft :: RealFloat a => [Complex a] -> [Complex a]
fft xs = fftIter [0..(length xs - 1)] xs

ifftSum :: RealFloat a => [Complex a] -> [Complex a] -> [Complex a] -> [Complex a]
ifftSum evenElems factors oddElems = zipWith (+) evenElems $ zipWith (*) factors oddElems

ifftIter :: RealFloat a => [Int] -> [Complex a] -> [Complex a]
ifftIter ks [] = [] -- bad
ifftIter ks [x] = replicate (length ks) x
ifftIter ks xs = do
    let evenElems = evenIdxs xs
    let oddElems = oddIdxs xs
    let factors = map (\k -> exp(0 :+ (2 * pi * (fromIntegral k) / fromIntegral (length xs)))) ks
    let evenRes = ifftIter ks evenElems
    let oddRes = ifftIter ks oddElems
    ifftSum evenRes factors oddRes

-- Currently ifft only operates on powers of 2
ifft :: RealFloat a => [Complex a] -> [Complex a]
ifft xs = do 
    let res = ifftIter [0..(length xs - 1)] xs
    map (\sum -> ((1 / fromIntegral (length xs)) :+ 0) * sum) res
