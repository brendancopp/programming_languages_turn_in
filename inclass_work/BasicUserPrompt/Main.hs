module Main where


--Defines main as and IO object
main :: IO ()
main = do
	promptForFile 
	filePath <- getLine

  stringContents = readFile filePath
  upperStringContents = toUpper stringContents
  
  writeFile filePath upperStringContents
    
    
promptForFile :: IO ()
promptForFile = putStrLn "What file would you like my guy"
	
