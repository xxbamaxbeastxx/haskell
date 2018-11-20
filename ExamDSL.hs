{-Andrew Wallace
ID: 10642754
CSci 450-01: Org. of Programming Languages
Assignment 4-}

{-# LANGUAGE InstanceSigs #-}
module ExamDSL 
where
import SimpleHTML
import Data.List
import System.IO
import Text.Printf
import Data.Function (on)
import Data.List (sortBy)

type QText   = String
type Tag     = String
data Question = Ask [Tag] QText [Choice] deriving Show

type AText   = String
data Choice  = Answer AText Bool deriving (Eq, Show)

type Title = String
data Exam = Quiz Title [Question] deriving Show
--exercise 6
instance Eq Question where
   (==) :: Question -> Question -> Bool
   (Ask ts1 tx1 choices1) == (Ask ts2 tx2 choices2) = let x = (removeDuplicates ts1) in
                                                      let y = (removeDuplicates ts2) in
                                                      let z = (removeDuplicates choices1) in
                                                      let w = (removeDuplicates choices2) in
                                                      eqBag x y && eqBag z w && tx1 == tx2


--exercise 1 
--determines if a choice is correct
correctChoice :: Choice -> Bool
correctChoice (Answer tx bo) = bo

--exericse 2
--returns the number of answers
lenQuestion :: Question -> Int
lenQuestion (Ask [] tx []) = 0
lenQuestion (Ask ids tx choices) = length choices

--exercise 3, returns whether a question is valid or not based on: a non-nil text
--at least 2 and no more than 10 possible answers
--exactly one correct answer
validQuestion :: Question -> Bool
validQuestion (Ask id tx choices)
    | length tx == 0 = False
    | (length choices) < 2 && (length choices) > 10 = False
    | oneRight choices == 1 = True
    | otherwise = False
--checks the number of right answers 
oneRight :: [Choice] -> Int
oneRight []     = 0 
oneRight ((Answer t x):xs)
    | x == True = (oneRight xs) + 1
    | otherwise = oneRight xs + 0
--exericse 4
--checks if a tag is in the question tags
hasTag :: Question -> Tag -> Bool
hasTag (Ask [] tx choices) t    = False
hasTag (Ask (x:xs) tx choices) t
    | x == t       = True
    | x /= t       = hasTag (Ask xs tx choices) t
--exercise 5
--compares if two lists have the same elements regardless of order or repeats
eqBag :: Eq a => [a] -> [a] -> Bool
eqBag [] []    = True
eqBag [] _     = False
eqBag _  []    = False
eqBag (x:xs) ys
    | inBoth x ys = eqBag xs (delete x ys)
    | otherwise = False

{-original solution with exhaustive calls on inBoth
eqBag :: Eq a => [a] -> [a] -> Bool
eqBag [] []    = True
eqBag [] _     = False
eqBag _  []    = False
eqBag (x:xs) (y:ys) -- possible fix, remove duplicates check inBoth on one and list length
    | inBoth x (y:ys) && inBoth y (x:xs) && eqBag xs ys = True
-}
--breaks apart the exam and calls a function to return all the questions with the same tags as the input list of tags
--exercise 7
selectByTags :: [Tag] -> Exam -> Exam
selectByTags xs (Quiz tit qs)  = (Quiz tit (tagsPulled xs qs))
--takes a list of questions and a list of tags and returns a list of questions with only guestions that have a tag in tags list
tagsPulled :: [Tag] -> [Question] -> [Question]
tagsPulled [] qs               = []
tagsPulled _ []                = []
tagsPulled (x:xs) qs          = (eachQ x qs) : tagsPulled xs qs
--exercise 8
--determines if an exam is valid by checking to see if each question is valid 
validExam :: Exam -> Bool
validExam (Quiz t qs)     = allQValid qs
--exercise 9
--makes a list of tupled pairs as an answer key to the given exam
makeKey :: Exam -> [(Int,Char)]
makeKey (Quiz t qs)            = key qs 1

--part b solution that returns a question as html
question2html :: Question -> HTML
question2html (Ask _ tx cs)      = to_li (tx ++ "\n" ++ (to_list UpLettered (listofChoices cs)) ++ "\n") 
--converts a list of choices to a string of properly formatted html li 
listofChoices :: [Choice] -> HTML
listofChoices []          = "\n"
listofChoices (x:xs)      = "\n" ++ choice2html x ++ listofChoices xs

--converts a choice to html
choice2html :: Choice -> HTML
choice2html (Answer text _) = to_li text

--takes a list of questions and packs the order followed by the letter of the correct answer into a tuple
key :: [Question] -> Int -> [(Int, Char)]
key [] _              = []
key (q:qs) num        = (num, letter(returnChoice q 1)) : key qs (num + 1)
                                 
--returns the number of the correct choice from the question
returnChoice :: Question-> Int -> Int
returnChoice (Ask t qt []) num   = 0
returnChoice (Ask t qt (c:cs)) num
    | correctChoice c == True = num
    | correctChoice c == False = returnChoice (Ask t qt cs) (num + 1)
    | otherwise = 0

--returns whether each question in a list of questions is valid
allQValid :: [Question] -> Bool
allQValid []            = True
allQValid (q:qs)        = validQuestion q && allQValid qs


--checks each tag to see if theres a match in the questions
eachQ :: Tag -> [Question] -> Question
eachQ t (q:qs)
    | hasTag q t == True = q
    | hasTag q t == False = eachQ t qs

--checks if an element is in a list 
inBoth ::Eq a => a -> [a] -> Bool
inBoth x []    = False
inBoth x (y:ys)
    | x == y = True
    | x /= y = inBoth x ys
    | otherwise = False

--removes duplicate elements from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rd []
    where rd dup [] = dup
          rd dup (x:xs)
              | x `elem` dup = rd dup xs
              | otherwise = rd (dup ++ [x]) xs
--pattern matches 1 to 26 to letters			  
letter :: Int -> Char
letter x
   | x == 1 = 'A'
   | x == 2 = 'B'
   | x == 3 = 'C'
   | x == 4 = 'D'
   | x == 5 = 'E'
   | x == 6 = 'F'
   | x == 7 = 'G'
   | x == 8 = 'H'
   | x == 9 = 'I'
   | x == 10 = 'J'
   | x == 11 = 'K'
   | x == 12 = 'L'
   | x == 13 = 'M'
   | x == 14 = 'N'
   | x == 15 = 'O'
   | x == 16 = 'P'
   | x == 17 = 'Q'
   | x == 18 = 'R'
   | x == 19 = 'S'
   | x == 20 = 'T'
   | x == 21 = 'U'
   | x == 22 = 'V'
   | x == 23 = 'W'
   | x == 24 = 'X'
   | x == 25 = 'Y'
   | x == 26 = 'Z'