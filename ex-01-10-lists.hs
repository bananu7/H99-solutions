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

