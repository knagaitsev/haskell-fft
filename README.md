## HaskellFFT

A simple library to do Fourier Transforms in Haskell

To run tests simply run:

```
cabal run test
```

The interface is below. Note: `fft` and `ifft` expect power of 2 length input. `Utils` includes `padPow2` in case this is needed

```
dftNaive :: RealFloat a => [Complex a] -> [Complex a]
idftNaive :: RealFloat a => [Complex a] -> [Complex a]
fft :: RealFloat a => [Complex a] -> [Complex a]
ifft :: RealFloat a => [Complex a] -> [Complex a]
```

Other useful utility functions can be found in `Util.hs`
