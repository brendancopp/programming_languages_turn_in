--====Unfaithful Sieve====

primes = sieve [2..10]
sieve (p:xs) = p : sieve [x | x <- xs, x mod p > 0]
2 [3..10]      2         [3,5,7,9]
3 [5,7,9]      3         [5,7,9]
5 [7,9]        5         [7,9]
7 [9]          7         [9]
9 []           9         [] 


--====Trial Division====
primes = 2 : [x | x <− [3..], isprime x]
isprime x = all (\p −> x ‘mod‘ p > 0) (factorsToTry x)
  where
    factorsToTry x = takeWhile (\p −> p*p <= x) primes\


--====Faithful Sieve (With priority queue implementation)====
--===========================================================

--Tree, Source: https://www.codementor.io/haskell/tutorial/monoids-fingertrees-implement-abstract-data
data Tree v a = Leaf   v a
  | Branch v (Tree v a) (Tree v a)

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

head :: Tree v a -> a
head (Leaf _ a)     = a
head (Branch _ x _) = head x

type Size = Int

tag (Leaf  ..)       = 1
tag (Branch .. x y)  = tag x + tag y


leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

(!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !! 0 = a
(Branch _ x y)  !! n 
     | n < tag x     = x !! n
     | otherwise     = y !! (n - tag x)

--Priority Queue, Source: https://www.codementor.io/haskell/tutorial/monoids-fingertrees-implement-abstract-data
type Priority = Int

tag (Leaf .. a)     = priority a
tag (Branch .. x y) = tag x `min` tag y

winner :: Tree Priority a -> a
winner t = go t
    where
    go (Leaf _ a)        = a
    go (Branch _ x y)
        | tag x == tag t = go x   -- winner on left
        | tag y == tag t = go y   -- winner on right


sieve [] = []
sieve (x:xs) = x : sieve’ xs (insertprime x xs PQ.empty)
  where
    insertprime p xs table = PQ.insert (p*p) (map (* p) xs) table
    sieve’ [] table = []
    sieve’ (x:xs) table
      | nextComposite <= x = sieve’ xs (adjust table)
      | otherwise = x : sieve’ xs (insertprime x xs table)
        where
          nextComposite = PQ.minKey table
          adjust table
            | n <= x = adjust (PQ.deleteMinAndInsert n’ ns table)
            | otherwise = table
            where
              (n, n’:ns) = PQ.minKeyValue table