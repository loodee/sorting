import Test.QuickCheck
import Sorting
import Data.List

main =
    do
     quickCheckWithResult stdArgs prop_quick_length >>= crash
     quickCheckWithResult stdArgs prop_quick_elem >>= crash
     quickCheckWithResult stdArgs prop_quick_head >>= crash

crash :: Result -> IO()
crash Success {} = return ()
crash _ = error "Failed test"

prop_quick_length :: [Int] -> Bool
prop_quick_length xs = (length xs) == length (quicksort xs)

prop_quick_elem :: [Int] -> Bool
prop_quick_elem (x:xs) = elem x (quicksort (x:xs))

prop_quick_head :: [Int] -> Bool
prop_quick_head xs = 0 == head( quicksort (0:xs) )
