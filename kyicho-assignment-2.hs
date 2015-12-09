{- Question 1 -}
duple :: Integer -> a -> [a]
duple 0 x = []
duple 1 x = [x]
duple n x = x : duple (n-1) x

{- Question 2 -}
invert :: [[a]] -> [[a]]
f [] = []
f [x,y] = [y,x]
f [x,y,z] = [x,y,z]
invert l = map f l

{- Question 3 -}
triplize :: [a] -> [a]
triplize [] = []
triplize l = head l : head l : head l : triplize (tail l)

{- Question 4 -}
-- Example: listSet ["a", "b", "c", "d"] 2 "e" = ["a","e","c","d"]
listSet :: [a] -> Integer -> a -> [a]
listSet [] n y = []
listSet l n y = take (fromIntegral n-1) l ++ [y] ++ drop (fromIntegral n) l 

{- Question 5 -}
prod :: [a] -> [b] -> [(a,b)]
touple x [] = []
touple x ys = (x, head ys) : touple x (tail ys)
prod [] ys = []
prod xs [] = []
prod xs ys = touple (head xs) ys ++ (prod (tail xs) ys)

 {- Question 6 -}
 -- Example: [1,2,3] -> [[1],[2],[3]]
down :: [a] -> [[a]]
m x = x : []
down [] = [[]]
down xs = map m xs 

{- Question 7 -}
-- Example: union [[1,2,3], [4,5], [6]] = [1,2,3,4,5,6]
union :: [[a]] -> [a]
flat xs = xs
union [] = []
union l = flat (head l) ++ union (tail l) 

{- Question 8 -}
ap f g x = f x (g x)
proj x y = x
identity :: a -> a
identity x =  proj (ap proj proj x) x

{- Question 9 & 10 -}
charIn ::  Char -> String -> Bool
charIn _ []       = False
charIn x (y:ys)   = x==y || charIn x ys

midfunc x = charIn x ['a'..'z']

isWord :: String -> Bool
--isWord str = all (`elem` ['a'..'z']) str
isWord str = all (charIn str) ['a'..'z']

prefixWords :: String -> [String] -> [String]
prefixWords str xs = map (str ++) (filter isWord xs)