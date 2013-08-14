Exercises from Chapter 2 of Real World Haskell

> lastButOne :: [a] -> a
> lastButOne a = last (init a)

Exercises from Chapter 3 of Reql World Haskell

> data List a = Cons a (List a)
>             | Empty
>             deriving (Show)

> fromList :: [a] -> List a
> fromList [] = Empty
> fromList (x:xs) = Cons x (fromList xs)

> toList :: List a -> [a]
> toList Empty = []
> toList (Cons x xs) = x:(toList xs)
