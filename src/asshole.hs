{-# LANGUAGE BangPatterns #-}
import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.HashTable.ST.Basic as HT
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Arbol a = Nada | Nodo (IORef (Arbol a)) (IORef (Arbol a)) (IORef (Arbol a)) a a deriving Show
instance Show (IORef a) where
    show _ = "<ioref>"

type MapaArbolinCaca s a = HT.HashTable s Int (Arbol a)
type MapaArbolins s a = ST s (MapaArbolinCaca s a)

threshold = 100 :: Int

scores :: MapaArbolins s a
scores = do 
  ht <- HT.new 
  HT.insert ht 1 Nada
  HT.insert ht 2 Nada
  HT.insert ht 3 Nada
  return ht

aboveThresholdST :: MapaArbolinCaca s a -> Int -> ST s Bool
aboveThresholdST radio run = do
  let ht = radio 
  cnt <- HT.lookup ht run
  let result = case cnt of
                 Nothing -> False
                 Just x -> True
  return result

assshit :: MapaArbolinCaca s a-> Int -> Arbol a -> ST s Int
assshit ht cadena valor = do 
    HT.insert ht cadena valor
    return 1

quemierda :: (Show a) =>IORef (Arbol a)-> Arbol a-> Bool
quemierda fuck bitch = unsafePerformIO$do
    print "pero q mierdas"
    vale<-readIORef fuck
    print ("escribiendo a "++" antes "++(show vale)++" despues "++(show bitch))
    writeIORef fuck bitch
    vale<-readIORef fuck
    print ("escribido "++(show vale))
    return True
quemierda _ _  = unsafePerformIO$do
    print "no mames"
    return True

pinche_mamada::(Ord a)=>a->a->Arbol a
pinche_mamada x idx = unsafePerformIO$do
    mierda1<-newIORef(Nada)
    mierda2<-newIORef(Nada)
    mierda3<-newIORef(Nada)
    let laputa=(Nodo (mierda1) (mierda2) (mierda3) x idx)
    return laputa

pero_la_puta_madre::(Show a)=>Arbol a->Bool
pero_la_puta_madre arbol=unsafePerformIO$do
    let (Nodo ref_mierda _ _ _ _ )=arbol
    pendejo<-(readIORef ref_mierda)
    print ("leido "++(show pendejo)++" de "++(show arbol))
    let chingadera= case pendejo of Nada -> False
                                    (Nodo _ _ _ _ _)-> True
    return chingadera

pero_la_puta_madre_hi:: (Show a) =>Arbol a->Bool
pero_la_puta_madre_hi arbol=unsafePerformIO$do
    let (Nodo _ ref_mierda _ _ _ )=arbol
    pendejo<-(readIORef ref_mierda)
    print ("pinche mamada "++(show pendejo))
    let chingadera= case pendejo of Nada -> False
                                    (Nodo _ _ _ _ _)-> True
    return chingadera

mierda :: Bool
mierda =  runST$do
    maputo<-scores
    let arbol4=pinche_mamada 40 4
        arbol5=pinche_mamada 50 5
        arbol6=pinche_mamada 60 6
        (Nodo _ arbol4_hi _ _ _)=arbol4
        (Nodo arbol5_p _ arbol5_hdp _ _)=arbol5
        (Nodo arbol6_p _ _ _ _)=arbol6
        !putada=(quemierda arbol4_hi arbol5)
        !chingada=(quemierda arbol5_p arbol4)
        chingadera=pero_la_puta_madre_hi arbol4
        ijadeputa=pero_la_puta_madre arbol5

--    let maputo=scores
--    maputo1<-(maputo)
    aaa<-aboveThresholdST maputo 1
--    aaa1 <- HT.lookup maputo "run1"
--    let aaa = case aaa1 of
--                      Nothing -> False
--                      Just x -> True
--    let maputo1=(maputo)
    HT.insert maputo 4 arbol4

    bbb<-aboveThresholdST maputo 4
--    bbb1 <- HT.lookup maputo "run4"

    assshit maputo 5 arbol5

    ccc<-aboveThresholdST maputo 5
--    ccc1 <- HT.lookup maputo "run5"

    assshit maputo 6 arbol6

    ddd<-aboveThresholdST maputo 6
--    ddd1 <- HT.lookup maputo "run6"

    let resu=(((((aaa&&bbb)&&ccc)&&ddd) && chingadera) )&&ijadeputa
    return resu

main = do
    print "oi"
    let puta=mierda
    print ("vide kill "++(show puta))
