module Lib.Array where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> [a]
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return xs')
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

splitFrom :: Int -> Int -> [a] -> ([a], [a])
splitFrom from to xs = ((slice from to xs), (slice (to+1) (length xs) xs))

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs