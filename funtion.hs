lucky::(Integral a)=>a->String
lucky 7 = "is lucky number 7"
lucky x = "you are not lucky"

sayMe::(Integral a)=>a->String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe 6 = "six"
sayMe x = "not between 1 and 6"

factorial :: (Integral a)=>a->a
factorial 0 = 1
factorial n = n * factorial(n - 1)

first::(a,b,c)->a
first (x,_,_) = x

second::(a,b,c)->b
second (_,x,_) = x

third::(a,b,c)->c
third (_,_,x) = x

length' :: (Num b) => [a]->b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a]->a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a)=>a->a->String
bmiTell weight height  
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're normal"
    | bmi <= fat = "You're fat"
    | otherwise   = "You're a whale"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5,25.0,30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

myCompare :: (Ord a)=> a->a->Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT


-- let it be : let [bindings] in [expressions]
cylinder :: (RealFloat a) => a->a->a
cylinder r h =    
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea


desList :: [a]->String
desList xs = "The list is " ++ case xs of [] -> "empty."
                                          [x] -> "a singleton list"
                                          xs -> "a longer list"

