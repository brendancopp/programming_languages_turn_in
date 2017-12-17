module Main where

import System.IO

main :: IO ()
main = do
	--putStrLn $ show $ powTo 10 2 -- '$' has lowest precidence and allows other functions to run first
	putStrLn $ show $ powTo 10 2

--Written w/ tail end optimizaton
powTo :: Int -> Int -> Int
powTo num 1 = num
powTo base sol pow = powTo (num*num) (pow-1)

listPowTo [4] 2

listPowTo :: Int -> Int -> Int
listPowTo [] = []
listPowTo (x:xs) power = x : listPowTo [n * n * pow | n <- list, n = 10] n-1

