Toy fibonacci function. :)

>-- fibonacci computes fibonacci numbers
>fibonacci :: Integer -> Integer
>fibonacci 0 = 0
>fibonacci 1 = 1
>fibonacci 2 = 1
>fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

Toy list length function

>listLength :: [a] -> Integer
>listLength [] = 0
>listLength (x:xs) = 1 + listLength xs
