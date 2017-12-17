cartProduct xs ys = [(x,y)| x <- xs, y <- ys]
cartProductEq xs ys = [(x,y)| x <- xs, y <- ys, x == y]

--isNotFirstFive var = case var of {[1,2,3,4,5] -> if var==0 then True else False}

data Person = Person{ 
firstName :: String,
lastName :: String,
age :: Int,
phone :: Int,
address :: String}	deriving(Show)

createPerson = Person "Brendan" "Copp" 20 9174344493 "66 Milbank"