module Main where

import Data.Char
import System.IO

--Defines main as and IO object
main :: IO ()
main = do
	promptForInput
	inputString <- getLine

	putStrLn $ map toUpper inputString
 
    
promptForInput :: IO ()
promptForInput = putStrLn "What word would you like me to upper my guy?"
	
