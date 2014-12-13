lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

toDigits :: Integer -> [Integer]
toDigits n
    | n<=0 = []
    | otherwise = toDigits(dropLastDigit n) ++ [lastDigit n]

auxDoubleEveryOther :: [Integer] -> [Integer]
auxDoubleEveryOther [] = []
auxDoubleEveryOther (x:[]) = [x]
auxDoubleEveryOther (x:y:xs) = x:2*y:auxDoubleEveryOther xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse(auxDoubleEveryOther(reverse l))

sumSingleDigit :: Integer -> Integer
sumSingleDigit n
    | n >= 10 = (lastDigit n) + (dropLastDigit n)
    | otherwise = n

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (head:tail) = (sumSingleDigit head) + (sumDigits tail)

validate :: Integer -> Bool
validate n = (mod ((sumDigits.doubleEveryOther.toDigits) n) 10) == 0
    
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end buffer
    | n == 1 = [(start,end)]
    | n == 2 = [(start,buffer),(start,end),(buffer,end)]
    | otherwise = (hanoi (n-1) start buffer end) ++ [(start,end)] ++ (hanoi (n-1) buffer end start)
