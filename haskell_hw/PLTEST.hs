arr1 = [1,2,3,4,5]
arr2 = [6,7,8,9,10]

spliceList [] [] = []
spliceList (x:xs) arr = x : spliceList arr xs

testStr = "aaatest"

-- Counts number of
countChar (c:cs) =  