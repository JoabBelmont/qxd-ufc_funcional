{-
    tail [1,2,3,4,5] -> [2,3,4,5]
    length [1,2,3,4,5] -> 5
    take (5 - 2) [2,3,4,5] -> [2,3,4]

    l = [1,2,3,4,5]
    crop l = take (length l - 2) (tail l) -> [2,3,4]
-}

crop :: [a] -> [a]
crop l = take (length l - 2) (tail l)
