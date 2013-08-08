-- Solutions to H99 problems
--
import System.Random
--
-- Exercises 1 to 28: Lists
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

--Ex 17
split :: Integral c => [a] -> c -> ([a],[a])

helper :: Integral c => [a] -> c -> [a] -> ([a],[a])
helper front n back = if n == 0 then (front, back)
                                else helper (front ++ [head back]) (n-1) (tail back)

split list c = helper [] c list

--Ex 18
slice :: Int -> Int -> ([a] -> [a])
slice i k = take (k-i+1) . drop (i-1)

--Ex 19
rotate :: Int -> [a] -> [a]
rotate c list@(x:xs) | c == 0 = list
                     | c < 0 = rotate (length list + c) list 
                     | c > 0 = rotate (c-1) (xs ++ [x])

--Ex 20
removeAt :: Int -> [a] -> (a, [a])
removeAt c list = (head (drop (c-1) list), (take (c-1) list) ++ (drop c list))

--Ex 21
insertAt :: Int -> a -> [a] -> [a]
insertAt c e xs = f ++ e : s
    where f = take (c-1) xs
          s = drop (c-1) xs

--Ex 22
range :: Int -> Int -> [Int]
range a b = [x | x <- [a..b]]

--Ex 23
rndSelect :: Int -> [a] -> [a]
rndSelect 0 _  = []
rndSelect c xs = foldl fun [] $ take c $ randomRs (1, length xs) (mkStdGen seed)
    where fun acc r = (elementAt xs r) : acc
          seed = 42

-- exercise 23 variation with generator passing
rndSelect' :: (RandomGen g) => Int -> [a] -> g -> ([a], g)
rndSelect' 0 _ g = ([], g)
rndSelect' n xs g = ((at i) : ys, g'')
    where at = elementAt xs
          (i, g') = randomR (1, length xs) g
          (ys, g'') = rndSelect' (n-1) xs g'

