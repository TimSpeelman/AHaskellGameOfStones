module Stones.Util
    where

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as = bs
    where
        -- withI :: [(a, Int)]
        withI = zip as [0..(length as)]
        -- f' :: (a, Int) -> b
        f' = \(a, i) -> f i a
        bs = map f' withI

-- First index is for the first layer, second for the second
mapWithIndex2D :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapWithIndex2D f rows = rows'
    where
        -- withI :: [([a], Int)]
        withI = zip rows [0..(length rows)]
        -- f' :: ([a], Int) -> [b]
        f' = \(cols, i) -> 
            mapWithIndex (\j cell -> f i j cell) cols
        rows' = map f' withI


setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs
        
join :: String -> [String] -> String
join sep xs = foldr (\a b-> a ++ if b=="" then b else sep ++ b) "" xs
