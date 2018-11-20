{-Andrew Wallace
ID: 10642754
CSci 450-01: Org. of Programming Languages
Assignment 1
-}
module HW01  -- in file nameed HW01.hs
   ( prodSqSmall, mulSq, addSq, xor, implies, ccArea, addTax, subTax,
     validDay, roman)
where
import Data.List
import System.IO
import Text.Printf

-- ELIFP Ch. 5, Ex. 2
--returns the product of the squares of the two smaller numbers.
prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall x y z
    | x >= y && x >= z = addSq y z
    | y >= x && y >= z = addSq x z
    | otherwise        = addSq x y
-- ELIFP Ch. 5, Ex. 3
--returns a square
mulSq :: Double -> Double
mulSq n              = n ^ 2
--returns the multiplication
addSq :: Double -> Double -> Double
addSq a b              = mulSq (a * b)

--returns an exclusive or
xor :: Bool -> Bool -> Bool
xor q w                = q /= w

-- ELIFP Ch. 5, Ex. 4
implies :: Bool -> Bool -> Bool
implies a b              = (not a) || b

-- ELIFP Ch. 5, Ex. 7
ccArea :: Double -> Double -> Double
ccArea x y             
    |x >= y              = (((x/2)^2) * pi) - (((y/2)^2) * pi)
    |y >= x              = (((y/2)^2) * pi) - (((x/2)^2) * pi)
    
-- ELIFP Ch. 5, Ex. 9
-- Consider the equation:  addTax c p = ct 
addTax :: Double -> Double -> Double
addTax x y                      = x + (x * (y/100))

subTax :: Double -> Double -> Double
subTax x y                      = x * (100 / (y + 100))

-- ELIFP Ch. 5, Ex.11
-- Using proleptic Gregorian calendar, which underlies ISO 8601 standard
validDay :: (Int,Int,Int) -> Bool
validDay d
    | (vMonth(d) && vDay(d) && vYear(d)) = True
    | otherwise                 = False
--decides if year is valid and if leap year is valid
vYear :: (Int, Int, Int) -> Bool
vYear d
    | thd'(d) >= 1 && thd'(d) <= 9999 && leapYear(d) == False && fst'(d) /= 2                    = True
    | thd'(d) >= 1 && thd'(d) <= 9999 && leapYear(d) == True && fst'(d) /= 2                     = True
    | thd'(d) >= 1 && thd'(d) <= 9999 && leapYear(d) == False && fst'(d) == 2 && snd'(d) <= 28   = True
    | thd'(d) >= 1 && thd'(d) <= 9999 && leapYear(d) == True && fst'(d) == 2 && snd'(d) <= 29    = True
    | otherwise                 = False
--checks if year is a leapYear
leapYear :: (Int, Int, Int) -> Bool
leapYear d =
    if thd'(d) `mod` 4 == 0
         then if thd'(d) `mod` 100 == 0
             then if thd'(d) `mod` 400 == 0
                 then True
                 else False
             else True
         else False
-- month, day, year
vMonth :: (Int, Int, Int) -> Bool
vMonth d          
    | fst'(d) >= 1 && fst'(d) <= 12 = True
    | otherwise                 = False
--checks if the month and day are valid
vDay :: (Int, Int, Int) -> Bool
vDay d
    | (fst'(d)==1 || fst'(d)==3 || fst'(d)==5 || fst'(d)==7 || fst'(d)==8 || fst'(d)==10 || fst'(d)==12) && (snd'(d) >= 1 && snd'(d) <= 31) = True
    | (fst'(d)==4 || fst'(d)==6 || fst'(d)==9 || fst'(d)==11 && snd'(d) >= 1 && snd'(d) <= 30) = True
    | fst' (d)==2               = True --validYear checks for leapYear
    | otherwise                 = False

--gets elements from triple tuple
fst' :: (Int, Int, Int) -> Int
fst' (x,_,_) = x
snd' :: (Int, Int, Int) -> Int
snd' (_,x,_) = x
thd' :: (Int, Int, Int) -> Int
thd' (_,_,x) = x

-- ELIFP Ch. 5, Ex. 12
-- Roman numerals in range [0-3999], where 0 represented as empty string
roman :: Int -> String
roman x
    | x > 3999                   = "Above 3999 is out of range"
    | x >= 1000                  = "M" ++ roman(x - 1000)
    | x >= 500                   = "D" ++ roman(x - 500)
    | x >= 100                   = "C" ++ roman(x - 100)
    | x >= 50                    = "L" ++ roman(x - 50)
    | x >= 10                    = "X" ++ roman(x - 10)
    | x >= 5                     = "V" ++ roman(x - 5)
    | x >= 1                     = "I" ++ roman(x - 1)
    | x < 0                      = "must be between 0-3999"
    | otherwise                  = ""