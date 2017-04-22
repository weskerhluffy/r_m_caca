import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.HashTable.ST.Basic as HT

type Score = Int
type ScoreCaca s = HT.HashTable s String Score
type Scores s = ST s (ScoreCaca s)

threshold = 100 :: Int

scores :: Scores s
scores = do 
  ht <- HT.new 
  HT.insert ht "run1" 100
  HT.insert ht "run2" 110
  HT.insert ht "run3" 98
  return ht

aboveThresholdST :: Scores s -> String -> ST s Bool
aboveThresholdST radio run = do
  ht <- radio 
  cnt <- HT.lookup ht run
  let result = case cnt of
                 Nothing -> False
                 Just x -> True
  return result

assshit :: HT.HashTable s String Score -> String -> Int-> ST s Int
assshit ht cadena valor = do 
    HT.insert ht cadena valor
    return 1

mierda :: Bool
mierda = runST $ do
--    maputo<-scores
    let maputo=scores
    aaa<-aboveThresholdST ((maputo)) "run1"
    let maputo1=runST(maputo)
    HT.insert maputo1 "run4" 456
    bbb<-aboveThresholdST maputo "run4"
    assshit maputo "run5" 678
    ccc<-aboveThresholdST maputo "run5"
    return True

main = do
    print "oi"
