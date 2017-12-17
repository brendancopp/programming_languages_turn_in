--Dictionary
module Store where

type Store = [(String, Integer)]

look :: Store -> String -> Maybe Integer
look [] str = Nothing
look ((key,val):dict) str = 
	if key == str
		then Just val
		else look dict str
		
update :: Store -> String -> Integer -> Store
update [] str n = [(str, n)]
update ((key,str):dict) str n =
	if key == str
		then (key, n):dict
		else update dict str n

