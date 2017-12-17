module Main where

import Data.Char
import System.IO

--Defines main as and IO object
main :: IO ()
main = do
	promptForFile 
	filePath <- getLine

	stringContents <- readFile filePath
	putStrLn $ map toUpper stringContents
 
    
promptForFile :: IO ()
promptForFile = putStrLn "What file would you like my guy"
	
