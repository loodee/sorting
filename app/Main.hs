module Main where

import Sorting

main :: IO ()
main = do
    putStrLn $ "mergesort: " ++ show (mergesort [4, 0, 12, 13, -7, -1, -1])
    
