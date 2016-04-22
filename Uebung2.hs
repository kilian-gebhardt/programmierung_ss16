module Uebung2 where

import Prelude hiding (unwords, words)

-- Übung 1
comp :: [Int] -> [Int] -> Bool
comp [] [] = True
comp [] _  = False
comp _  [] = False
-- comp (x:xs) (y:ys) = (x == y) && comp xs ys
comp (x:xs) (y:ys) | x == y    = comp xs ys
                   | otherwise = False


merge :: [Int] -> [Int] -> [Int]
--merge [] [] = []
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) | a <= b    = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

-- Übung 2
unwords :: [String] -> String
unwords []         = ""
unwords [w]        = w
unwords (w1:w2:ws) = w1 ++ ' ' : unwords (w2:ws)

words :: String -> [String]
words "" = []
words s  = let (w, rs) = go "" s
           in  w : words rs

go :: String -> String -> (String, String)
go w ""         = (w, "")
go w (' ':inp') = (w, inp')
go w ( l :inp') = go (w ++ [l]) inp'

-- Übung 3
data Tree = Leaf Int | Branch Tree Tree deriving Show

t = Leaf 5
t' = Branch (Leaf 5) (Leaf 6)
t'' = Branch ( Branch (Leaf 1) (Leaf 2) )
             ( Branch (Leaf 3)
             ( Branch (Leaf 4) (Leaf 5))
             )

toList :: Tree -> [Int]
toList (Leaf a) = [a]
toList (Branch b c) = toList b ++ toList c

-- Übung 4: Teillösung
-- berechne zunächst nur die Quadrate der
-- geraden Zahlen in der Eingabeliste
f' :: [Int] -> [Int]
f' xs = map (^2) (filter even xs)





























































