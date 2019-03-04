
-- 柯里函数
mulThree :: (Num a) => a->a->a->a
mulThree x y z = x * y * z

-- compare :: Ord a => a -> a -> Ordering
compareWithHundred :: (Num a, Ord a)=> a->Ordering
compareWithHundred = compare 100

-- (/) :: Fractional a => a -> a -> a
divideByTen :: (Floating a) => a->a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- 高阶函数
applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c)->b->a->c
flip' f x y = f y x

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' p (x:xs)
     | p x   = x : filter' p xs
     | otherwise = filter' p xs

quicksort :: (Ord a) => [a]->[a]
quicksort [] = []
quicksort (x:xs) = 
    let smallsort = quicksort (filter' (<= x) xs)
        largesort = quicksort (filter' (> x) xs)
    in smallsort ++ [x] ++ largesort

largestDivisible :: (Integral a) => a 
largestDivisible = head (filter' p [100000,99999..])
      where p x = x `mod` 3828 == 0

chain :: (Integral a) => a->[a]
chain 1 = [1]
chain n
    | even n = n : chain(n `div` 2)
    | odd n = n : chain(n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs->length xs > 15) (map chain [1..100]))

flip'' :: (a->b->c)->b->a->c
flip'' f = \x y -> f y x

-- foldl 应用
-- sum' :: (Num a) => [a]->a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs

sum' :: (Num a) => [a]->a
sum' = foldl (*) 1

elem' :: (Eq a) =>a->[a]-> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a->b) ->[a]->[b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a]->a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

-- reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []

reverse' :: [a] -> [a]
reverse' = foldl (flip(:)) []

product' :: (Num a) => [a]->a
product' = foldl1(*)

filter'' :: (a->Bool) -> [a] ->[a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a]->a
head' = foldr1 (\x _ -> x)

last' :: [a]->a
last' = foldl1 (\_ x -> x)

