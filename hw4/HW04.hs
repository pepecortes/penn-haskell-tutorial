module HW04 where

ex1 :: a -> b -> b
ex1 i j = j
--no restriction for the type of a!

ex2 :: a -> a -> a
ex2 i j = i
--it could also yield j instead!


ex3 :: Int -> a -> a
ex3 i any = any
--is this the only possible implementation?

ex4 :: Bool -> a -> a -> a
ex4 test x y = if test then x else y
--either returns x or y. it cannot go any much longer

ex5 :: Bool -> Bool
ex5 test = not test
--returns true or false, obviously
--either returns the input or negates the input

--ex6 :: (a -> a) -> a
--ex6 f = ??? 
--impossible: no means of retrieving the argument of f until it is applied!

ex7 :: (a -> a) -> a -> a
ex7 f x = x
-- it would work for any function, as long as it is of type (a -> a)
-- example  ex7 (\x -> x) 'a'
-- but the only possible result is the second argument

ex8 :: [a] -> [a]
ex8 [] = []
ex8 (h:t) = t
-- many possible implementations, as long as the result returns a
-- list which elements are of the same type

ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
ex9 f xs = map f xs
-- it looks quite straight forward!

{-
ex10 :: Maybe a -> a
ex10 (Just x) = x
ex10 Nothing = Nothing
Cannot work: if the argument is 'Nothing' it is impossible
to return something of type 'a' out of thin air
-}

ex11 :: a -> Maybe a
ex11 x = Nothing
-- It would also work for the ex11 x = x
-- I just can see this two outputs but
-- the function could be more complex (like randomly selecting
-- one or the other

ex12 :: Maybe a -> Maybe a
ex12 (Just x) = Nothing
ex12 Nothing = Nothing
-- I cannot find anything interesting to say about this one
-- except that it returns the same input or Nothing

