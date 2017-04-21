module Lookup
where

import Control.Monad.ST.Safe
import qualified Data.HashTable.ST.Basic as HT

type Score = Int
type Scores s = ST s (HT.HashTable s String Score)

threshold = 100 :: Int

scores :: Scores s
scores = do 
  ht <- HT.new 
  HT.insert ht "run1" 100
  HT.insert ht "run2" 110
  HT.insert ht "run3" 98
  return ht

aboveThresholdST :: Scores s -> String -> ST s Bool
aboveThresholdST scores run = do
  ht <- scores
  cnt <- HT.lookup ht run
  let result = case cnt of
                 Nothing -> False
                 Just x -> (x > threshold)
  return result

-- Question #1 is this possible?? i.e. escape the ST s Bool result above?
-- Say by some sort of application of runST that I cannot get to work?
aboveThreshold :: Scores s -> String -> Bool
aboveThreshold scores run = undefined

-- Question #2 What I' really like to do, as an example, is use the Scores hashtable to do a simple filter.
-- e.g., filterAboveThreshold scores ["run1","run2","run3"] => ["run1","run2"]
filterAboveThreshold :: Scores s -> [String] -> [String]
filterAboveThreshold scores runs = undefined

-- I know I can always get to IO and write a 
-- filterAboveThreshold :: Scores s -> [String] -> IO [String] 
-- Is that the best one can do here?  Is there any stipulation of 's' besided 'RealWord' feasible?
aboveThresholdIO :: Scores RealWorld -> String -> IO Bool
aboveThresholdIO scores run = do
    let result = do
          ht <- scores
          cnt <- HT.lookup ht run
          return $ case cnt of
                     Nothing -> False
                     Just x -> (x > threshold)
    rs <- stToIO result
    return rs

main = print "oi"
