
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