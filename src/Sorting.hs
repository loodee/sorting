module Sorting 
    ( mergeSort
    ) where


-- | mergesort
mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left,right) = halve xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take lhx xs, drop lhx xs)
    where lhx = length xs `div` 2


-- | quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted