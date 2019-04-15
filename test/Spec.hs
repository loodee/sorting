import Test.QuickCheck
import Sorting
import Data.List

main =
    do
     quickCheckWithResult stdArgs prop_quick_length >>= crash
     quickCheckWithResult stdArgs prop_quick_elem >>= crash

crash :: Result -> IO()
crash Success {} = return ()
crash _ = error "Failed test"

prop_quick_length :: [Int] -> Bool
prop_quick_length xs = (length xs) == length (quicksort xs)

prop_quick_elem :: [Int] -> Bool
prop_quick_elem []     = True
prop_quick_elem (x:xs) = elem x (quicksort (x:xs))


