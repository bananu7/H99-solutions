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

--Ex 13
encodeDirect :: (Integral c, Eq a) => [a] -> [RleElem c a]

encodeElem :: Integral c => c -> a -> RleElem c a
encodeElem c a = if c > 1 then (Multiple c a)
                          else (Single a)

encodeInner :: (Integral c, Eq a) => (c,a) -> [a] -> [RleElem c a]
encodeInner (c,a) [] = [encodeElem c a]
encodeInner (c,a) (h:t)
    | a == h = encodeInner (c+1, a) t
    | otherwise = (encodeElem c a) : encodeInner (1,h) t

encodeDirect [] = []
encodeDirect (h:t) = encodeInner (1, h) t

--Ex 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--Ex 15
repli :: Integral c => [a] -> c -> [a]
repli [] _ = []
repli (x:xs) c = (replicate (fromIntegral c) x) ++ (repli xs c)

--Ex 16
dropEvery :: Integral c => [a] -> c -> [a]
-- I don't really like the foldr solution
--dropEvery list n = foldr (\(x,c) acc -> if c `mod` n == 0 then acc 
--                                                          else x:acc) [] (zip list [1..])
dropEvery list n = map fst $ filter (\(x,c) -> c `mod` n /= 0) (zip list [1..])
