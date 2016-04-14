module Uebung1 where

-- kilian.gebhardt@tu-dresden.de
-- https://github.com/kilian-gebhardt

-- Übung 1 a)
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- Übung 1 b)
sumFacs :: Int -> Int -> Int
sumFacs n m | n >  m = 0
            | n == m = fac n
            | n <  m = fac n + sumFacs (n+1) m

-- Übung 2)
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
--fib (n+2) = fib n     + fib (n+1)
fib n     = fib (n-2) + fib (n-1)

-- Übung 3
k :: Int -> Int
k 1 = 1
k n | n `mod` 2 == 0 = 1 + k (n `div` 2) -- n gerade
    | otherwise      = 1 + k (3 * n + 1) -- n ungerade

maxk :: Int -> Int
maxk 1 = k 1
maxk n = max' (maxk (n-1)) (k n)

max' :: Int -> Int -> Int
max' n m | n > m     = n
         | otherwise = m

bin 0 = 0
bin 1 = 1
bin n = summing 0 (n-1)
  where summing i 0 = bin i * bin 0
        summing i j = bin i * bin j + summing (i+1) (j-1)






























