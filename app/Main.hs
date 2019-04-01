module Main where

import Sorting

main :: IO ()
main = do
    putStrLn $ "mergeSort: " ++ show (mergeSort [4, 0, 12, 13, -7, -1, -1])
    
