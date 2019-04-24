import Test.QuickCheck
import Sorting
import Data.List

main = do
    -- quicksort properties
    quickCheckWithResult stdArgs prop_quick_sorted >>= crash
    quickCheckWithResult stdArgs prop_quick_length >>= crash
    quickCheckWithResult stdArgs prop_quick_elem >>= crash

    -- merge sort properties
    quickCheckWithResult stdArgs prop_merge_sorted >>= crash
    quickCheckWithResult stdArgs prop_merge_length >>= crash

crash :: Result -> IO ()
crash Success {} = return ()
crash _ = error "Failed test"

-- | Ensures that quicksort indeed sorts the list.
prop_quick_sorted :: [Int] -> Bool
prop_quick_sorted xs = isSorted $ quicksort xs

-- | Ensures that quicksort preserves the length of the list.
prop_quick_length :: [Int] -> Bool
prop_quick_length xs = (length xs) == length (quicksort xs)

-- | Ensures that quicksort preserves the elements of the list.
prop_quick_isPermmut :: [Int] -> Bool
prop_quick_isPermmut xs = xs `isPermutation` (quicksort xs)

-- | Ensures that mergesort indeed sorts the list.
prop_merge_sorted :: [Int] -> Bool
prop_merge_sorted xs = isSorted $ mergesort xs

-- | Ensures that mergesort preserves the length of the list.
prop_merge_length :: [Int] -> Bool
prop_merge_length xs = length xs == (length $ mergesort xs)

-- | Ensures that mergesort preserves the elements of the list.
prop_merge_isPermmut :: [Int] -> Bool
prop_merge_isPermmut xs = xs `isPermutation` (mergesort xs)

prop_quick_elem :: [Int] -> Bool
prop_quick_elem []     = True
prop_quick_elem (x:xs) = elem x (quicksort (x:xs))

-- | Checks if a given list is sorted
isSorted :: Ord a => [a] -> Bool
isSorted []     = True
isSorted (x:[]) = True
isSorted (x:xs) | x <= head xs = isSorted xs
                | otherwise    = False

-- | Ensures that every element in unsorted list is present in 
--   the sorted list
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

