module HaskellFFT.Util (allClose, createLinspace) where

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
