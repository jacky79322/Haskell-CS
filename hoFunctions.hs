
--multThree
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

--compare100
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 

--divideByTen
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 

--isUpperAlphanum
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

--applyTwice
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

--zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

--flip
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y 

--these two functions of map and filter are already implemented on ghci
--map
--map :: (a -> b) -> [a] -> [b]  
--map _ [] = []  
--map f (x:xs) = f x : map f xs  
--multiple occurance error on map since there are two (Prelude or main)
--map is already an implemented function on ghci 

--filter
--filter :: (a -> Bool) -> [a] -> [a]  
--filter _ [] = []  
--filter p (x:xs)   
--    | p x       = x : filter p xs  
--    | otherwise = filter p xs  

--quicksort
--quicksort :: (Ord a) => [a] -> [a]    
--quicksort [] = []    
--quicksort (x:xs) =     
--    let smallerSorted = quicksort (filter (<=x) xs)  
--        biggerSorted = quicksort (filter (>x) xs)   
--    in  smallerSorted ++ [x] ++ biggerSorted  

--largestDivisible
--largestDivisible :: (Integral a) => a  
--largestDivisible = head (filter p [100000,99999..])  
--    where p x = x `mod` 3829 == 0  

--chain
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

--numLongChains
--numLongChains :: Int  
--numLongChains = length (filter isLong (map chain [1..100]))  
--    where isLong xs = length xs > 15 
--numLongChains2
--numLongChains2 :: Int  
--numLongChains2 = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

--addThree
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  
--optional
--addThree :: (Num a) => a -> a -> a -> a  
--addThree = \x -> \y -> \z -> x + y + z  

--flip2
flip'2 :: (a -> b -> c) -> b -> a -> c  
flip'2 f = \x y -> f y x  

--sumwithfold
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  
--alternate
--sum' = foldl (+) 0 

--elem`2withfold
elem'2 :: (Eq a) => a -> [a] -> Bool  
elem'2 y ys = foldl (\acc x -> if x == y then True else acc) False ys 

--map2withfold
map'2 :: (a -> b) -> [a] -> [b]  
map'2 f xs = foldr (\x acc -> f x : acc) [] xs  

--lots of functions with folds
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

--sqrtSums
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  

--function application
($) :: (a -> b) -> a -> b  
f $ x = f x

--function composition
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 

--fn practice
--fn = ceiling . negate . tan . cos . max 50  

--oddSquareSum with func composition
oddSquareSum :: Integer  
--oddSquareSum = sum Main.. takeWhile (<10000) Main.. filter odd Main.. map (^2) $ [1..]  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  