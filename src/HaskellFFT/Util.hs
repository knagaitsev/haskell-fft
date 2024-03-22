module HaskellFFT.Util (allClose, createLinspace, getElem, setElem, distFromPow2, padPow2) where

allClose :: (Ord a, Num a) => [a] -> [a] -> a -> Bool
allClose [] [] _ = True
allClose (a:as) (b:bs) threshold = ((abs $ a - b) <= threshold) && (allClose as bs threshold)
allClose _ _ _ = False

createLinspaceIter :: RealFloat a => Int -> Int -> a -> a -> [a]
createLinspaceIter 0 _ _ _ = []
createLinspaceIter n tot a b = do
    let x = a + (b - a) * (fromIntegral (tot - n)) / (fromIntegral (tot))
    x : createLinspaceIter (n - 1) tot a b

createLinspace :: RealFloat a => Int -> a -> a -> [a]
createLinspace n = createLinspaceIter n n

getElemIter :: [a] -> Int -> Int -> Maybe a
getElemIter [] _ _ = Nothing
getElemIter (x:xs) n 0 = Just x
getElemIter (x:xs) n i = getElemIter xs n (i - 1)

getElem :: [a] -> Int -> Maybe a
getElem xs n = getElemIter xs n n

setElem :: [a] -> Int -> a -> [a]
setElem [] _ _ = []
setElem (_:xs) 0 v = v : xs
setElem (x:xs) n v = x : (setElem xs (n - 1) v)

distFromPow2Iter :: Int -> Int -> Int
distFromPow2Iter num comp
    | num <= comp = comp - num
    | otherwise = distFromPow2Iter num (comp * 2)

distFromPow2 :: Int -> Int
distFromPow2 num = distFromPow2Iter num 1

pad :: [a] -> a -> Int -> [a]
pad x padVal n = x ++ (replicate n padVal)

padPow2 :: [a] -> a -> [a]
padPow2 x padVal = do
    let dist = distFromPow2(length x)
    pad x padVal dist
