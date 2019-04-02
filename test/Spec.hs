import Test.QuickCheck
import Sorting
import Data.List

main = quickCheck prop_quick_length

prop_quick_length :: [Int] -> Bool
prop_quick_length xs = (length xs) == length (quicksort xs)
