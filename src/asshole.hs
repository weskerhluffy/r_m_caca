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

aboveThresholdST :: ScoreCaca s -> String -> ST s Bool
aboveThresholdST radio run = do
  let ht = radio 
  cnt <- HT.lookup ht run
  let result = case cnt of
                 Nothing -> False
                 Just x -> True
  return result

assshit :: ScoreCaca s-> String -> Int-> ST s Int
assshit ht cadena valor = do 
    HT.insert ht cadena valor
    return 1

mierda :: Bool
mierda =  runST$do
    maputo<-scores
--    let maputo=scores
--    maputo1<-(maputo)
    aaa<-aboveThresholdST maputo "run1"
--    aaa1 <- HT.lookup maputo "run1"
--    let aaa = case aaa1 of
--                      Nothing -> False
--                      Just x -> True
--    let maputo1=(maputo)
    HT.insert maputo "run4" 456

    bbb<-aboveThresholdST maputo "run4"
--    bbb1 <- HT.lookup maputo "run4"

    assshit maputo "run5" 678

    ccc<-aboveThresholdST maputo "run5"
--    ccc1 <- HT.lookup maputo "run5"

    assshit maputo "run6" 678

    ddd<-aboveThresholdST maputo "run6"
--    ddd1 <- HT.lookup maputo "run6"

    let resu=((aaa&&bbb)&&ccc)&&ddd
    return resu

main = do
    print "oi"
    let puta=mierda
    print ("vide kill "++(show puta))
