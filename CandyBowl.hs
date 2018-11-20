{-Andrew Wallace
ID: 10642754
CSci 450-01: Org. of Programming Languages
Assignment 3-}

module CandyBowl where
import Data.List
import System.IO
import Text.Printf
import Data.Function (on)
import Data.List (sortBy)



data CandyBowl a = Bowl [a] deriving Show
--creates a new empty bowl
newBowl :: CandyBowl a
newBowl = Bowl []
--determines if a bowl is empty
isEmpty :: CandyBowl a -> Bool
isEmpty (Bowl [])  = True
isEmpty otherwise  = False
--puts a piece of candy in the bowl
putIn :: CandyBowl a -> a -> CandyBowl a
putIn (Bowl x) y           = (Bowl (y : x))
--determines if a piece of candy is in the bowl
has :: Eq a => CandyBowl a -> a -> Bool
has (Bowl x) y              = elem y x
--returns the number of pieces in the bowl
size :: CandyBowl a -> Int
size (Bowl x)             = length x
--returns how many pieces of a particular candy are in a bowl
howMany ::Eq a => CandyBowl a -> a -> Int
howMany (Bowl x) y
       | has (Bowl x) y == False = 0
       | has (Bowl x) y == True = occurances x y
--recursively calls itself to count a candy with atleast 1 occurance	   
occurances :: Eq a => [a] -> a -> Int
occurances [] y = 0
occurances (x:xs) y
       | x==y = 1 + (occurances xs y)
       | otherwise = occurances xs y
--removes a piece of candy from the bowl
takeOut ::Eq a => CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut (Bowl x) y
       | has (Bowl x) y == False = Nothing
       | has (Bowl x) y == True  = Just (findCandyRemove x y)
--returns the candyBowl 
findCandyRemove :: Eq a => [a] -> a -> CandyBowl a
findCandyRemove (x:xs) y
       | x == y            = (Bowl xs)
       | x /= y            = findCandyRemove xs y
--determines if 2 bowls of candy are equal
eqBowl ::Ord a => Eq a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl x) (Bowl y)
       | size (Bowl x) /= size (Bowl y) = False
       | size (Bowl x) == size (Bowl y) = check (inventory (Bowl x)) (inventory (Bowl y))
--checks if each piece is the same in type and count
check ::Eq a => [(a,Int)] -> [(a,Int)] -> Bool
check (x:xs) (y:ys)
       | fst (x) == fst (y) && snd (x) == snd (y) && length xs > 0 && length ys > 0 = check xs ys
       | length xs == 0 && length ys == 0 = True
       | otherwise = False

--returns a list of pairs where the first term is the num of the second
inventory ::Ord a => CandyBowl a -> [(a,Int)]
inventory (Bowl x)
       | isEmpty (Bowl x) == True = []
       | size (Bowl x) == 1 = [((head x), 1)]
       | size (Bowl x) >= 2  = sortBy (compare `on` snd) (countGroups (Bowl x) (unique (Bowl x)))
       | otherwise = []
--calls unique and removes 
countGroups ::Ord a=> CandyBowl a -> [a] -> [(a, Int)]
countGroups (Bowl x) []     = []
countGroups (Bowl x) y      = ((head y), (howMany (Bowl x) (head y))) : countGroups (Bowl x) (tail y)
--unpacks the list from the bowl to find the unique elements
unique ::Eq a =>CandyBowl a -> [a]
unique (Bowl x)       = removeDuplicates x
--removes duplicate elements from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rd []
    where rd dup [] = dup
          rd dup (x:xs)
              | x `elem` dup = rd dup xs
              | otherwise = rd (dup ++ [x]) xs

--implemented #11 takes 2 candy bowls and combines them
combine :: CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl x) (Bowl y) = (Bowl (x ++ y))



--solution with occurances might work if howMany and occurances are fixed
{-sort (((head x), ((groupUp (sortBowl(Bowl x))))) : inventory (Bowl (drop (groupUp (sortBowl(Bowl x))) x)))
       | otherwise = []
--returns the number duplicates 
groupUp ::Ord a => CandyBowl a -> Int
groupUp (Bowl x) = (size (Bowl x) - (countGroup (sort (x))))
--returns the length of the remaining list
countGroup :: Eq a=> [a] -> Int
countGroup (x:y:xs)--only really checks for duplicates not multiple occurances
       | x == y       = countGroup (y:xs)
       | x /= y       = length (xs)  

sortBowl :: Ord a => CandyBowl a -> CandyBowl a
sortBowl (Bowl x)  = (Bowl (sort x))
-}