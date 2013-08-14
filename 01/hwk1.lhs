CIS 194 Homework 1: 
===================
Brendon Roberto

Part 1: Validating Credit Card Numbers

> validateCreditCardNumber :: Integer -> Bool
> validateCreditCardNumber n = ((sumDigitsList . transformDigits . toDigitsReverse $ n) `mod` 10) == 0


Step 1: Get a list of Integers from an Integer

> toDigitsReverse :: Integer -> [Integer]
> toDigitsReverse n
>        | n > 0 = (n `mod` 10) : toDigitsReverse (n `div` 10)
>        | otherwise = []

Step 3: Double every second digit in [Integer]

> transformDigits :: [Integer] -> [Integer]
> transformDigits [] = []
> transformDigits (x:y:xs) = x : (2 * y) : transformDigits xs
> transformDigits (x:xs) = x: transformDigits xs

Step 4: Sum every individual digit in [Integer]

> sumDigitsList :: [Integer] -> Integer
> sumDigitsList [] = 0
> sumDigitsList (x:xs) = sumDigits x + sumDigitsList xs

> sumDigits :: Integer -> Integer
> sumDigits n 
>           | n > 0 = (n `mod` 10) + sumDigits (n `div` 10)
>           | otherwise = 0

Part 2: Tower of Hanoi

> type Peg = String
> type Move = (Peg, Peg)
> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
> hanoi n source target storage 
>       | n > 1 = (hanoi (n - 1) source storage target) ++ (source, target) : (hanoi (n - 1) storage target source)
>       | otherwise = [(source, target)]
