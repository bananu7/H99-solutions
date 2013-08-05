-- Solutions to H99 problems
--
-- Exercises 1 to 10: Lists
--Ex 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) =
    myLast xs 

--Ex 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (x:xs) = 
    myButLast xs

--Ex 3
elementAt :: (Integral b) => [a] -> b -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) ix = elementAt xs (ix-1)

--Ex 4
myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (x:xs) = myLength xs + 1

--Ex 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl (\acc x -> x:acc) [] xs 

--Ex 6
myPalindrome :: Eq a => [a] -> Bool
myPalindrome [] = True
myPalindrome x = x == myReverse x

--Ex 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List inner) = foldl (\acc x -> acc ++ (myFlatten x)) [] inner

--Ex 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

--Ex 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list = let (left,right) = span (== head list) list in
            left : pack right

--Ex 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)

--Ex 11
data RleElem c a = Single a | Multiple c a deriving Show

encode' :: (Integral c, Eq a) => [a] -> [RleElem c a]
encode' list = map (\x -> case length x of 
                         1 -> Single (head x)
                         _ -> Multiple (fromIntegral $ length x) (head x))
                   (pack list)

--Ex 12
decode :: (Integral c) => [RleElem c a] -> [a]
decode list = foldr (\x acc -> case x of
                        Single elem -> elem:acc
                        Multiple count elem -> (replicate (fromIntegral count) elem) ++ acc) [] list

