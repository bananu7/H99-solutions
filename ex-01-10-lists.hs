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

