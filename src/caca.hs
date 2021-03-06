-- XXX: https://mail.haskell.org/pipermail/beginners/2012-April/009743.html
-- XXX: https://en.wikibooks.org/wiki/Haskell/Mutable_objects
-- XXX: http://wiki.c2.com/?HaskellExampleForMutabilityOnObjects
-- XXX: http://stackoverflow.com/questions/4418017/haskell-new-instance-declaration-for-show
-- XXX: http://blog.jakubarnold.cz/2014/07/20/mutable-state-in-haskell.html
-- XXX: https://news.ycombinator.com/item?id=1831403
-- XXX: http://stackoverflow.com/questions/8332307/show-for-io-types
-- XXX: https://wiki.haskell.org/Haskell_IO_for_Imperative_Programmers
-- XXX: http://stackoverflow.com/questions/8879391/how-do-i-convert-string-into-list-of-integers-in-haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.HashTable.ST.Basic as M
import Data.Hashable
import Data.List

data Arbol a = Nada | Nodo (IORef (Arbol a)) (IORef (Arbol a)) (IORef (Arbol a)) a a deriving Show

instance Show (IORef a) where
    show _ = "<ioref>"

type MapaArbolinCaca s a = M.HashTable s a (Arbol a)
type MapaArbolins s a = ST s (MapaArbolinCaca s a)

caca_genera_mapa::(Ord a)=> MapaArbolins s a
caca_genera_mapa = do
    ht<-M.new
    return ht

insertar :: (Ord a) => Arbol a-> a -> a -> Arbol a
insertar Nada x idx = unsafePerformIO $ do
    mierda1<-newIORef Nada
    mierda2<-newIORef Nada
    mierda3<-newIORef Nada
    return (Nodo (mierda1) (mierda2) (mierda3) x idx)
insertar ultimo_insertado@(Nodo padre t1 t2 v idx_padre) x idx = unsafePerformIO $ do
    let mierda=((insertar_nodo ultimo_insertado Nada x idx))
    return mierda

insertar_nodo :: (Ord a) => Arbol a -> Arbol a-> a -> a -> Arbol a
insertar_nodo Nada !anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = unsafePerformIO $ do
    mierda1<-newIORef Nada
    mierda2<-newIORef Nada
    mierda3<-newIORef Nada
    mierda4<-newIORef anterior
    let ass = (Nodo mierda1 mierda4 mierda2 x idx)
    writeIORef padre_ant ass
    return ass
insertar_nodo !actual@(Nodo !padre t1 t2 v idx_padre) !anterior x idx
    | x < v = unsafePerformIO $ do padre_ref<-readIORef(padre)
                                   let verga=insertar_nodo padre_ref actual x idx
                                   return verga
    | otherwise  = unsafePerformIO $ do t2_cont<-readIORef(t2)
                                        case t2_cont of Nada ->    do actual_ref<-newIORef actual
                                                                      mierda1<-newIORef Nada
                                                                      mierda2<-newIORef Nada
                                                                      let nuevo_nodo= Nodo actual_ref mierda1 mierda2 x idx
                                                                          t2_ref = t2
                                                                      writeIORef t2_ref nuevo_nodo
                                                                      return nuevo_nodo
                                                        t2_cont -> do actual_ref<-newIORef actual
                                                                      t2_cont_ref<-newIORef t2_cont
                                                                      mierda1<-newIORef Nada
                                                                      let nodo_nuevo = Nodo actual_ref t2_cont_ref mierda1 x idx
                                                                          t2_ref = t2
                                                                          (Nodo padre_t2 _ _ _ _ ) = t2_cont
                                                                          padre_t2_ref = padre_t2
                                                                      writeIORef padre_t2_ref nodo_nuevo
                                                                      writeIORef t2_ref nodo_nuevo
                                                                      return nodo_nuevo
insertar_nodo _ _ _ _ = Nada

caca_construye_arbol :: (Ord a, Num a, Hashable a) => [a] -> Arbol a -> a -> MapaArbolinCaca s a -> ST s (Arbol a)
caca_construye_arbol [] arbolin idx _ = do
    return arbolin
caca_construye_arbol (x:xs) !arbolin idx maputo = do
    let x_shit=(x+0)
        ultimo=(insertar arbolin x_shit idx)
--    M.insert maputo x_shit ultimo 
    M.insert maputo idx ultimo 
    putamierda<-caca_construye_arbol xs ultimo (idx+1) maputo
    return putamierda

encuentra_raiz :: (Ord a) => Arbol a -> Arbol a
encuentra_raiz Nada = Nada 
encuentra_raiz !actual@(Nodo padre _ _ x idx) = let padre_cont = unsafePerformIO(readIORef(padre))
                                                in case padre_cont of Nada -> actual
                                                                      padre_cont ->encuentra_raiz padre_cont
                                                                      
camino_raiz:: (Ord a) => Arbol a -> [(a,a)]
camino_raiz Nada = [] 
camino_raiz !actual@(Nodo padre _ _ x idx) = let padre_cont = unsafePerformIO(readIORef(padre))
                                                in case padre_cont of Nada -> [(x,idx)]
                                                                      padre_cont ->(camino_raiz padre_cont)++[(x,idx)]

inorder :: (Ord c, Num c) => Arbol c -> c -> [(c,c,c)]
inorder Nada _ = []
inorder (Nodo _ hi hdp x idx) derp = (inorder (unsafePerformIO(readIORef(hi))) (derp+1)) ++ [(x,idx,derp)] ++ (inorder (unsafePerformIO(readIORef(hdp))) (derp+1))

convertir_monton_mierda::[String]->[[Int]]
convertir_monton_mierda [] = []
convertir_monton_mierda (cad:cads) =
    case cads of []-> [(map read $ words cad:: [Int])]
                 cads-> [(map read $ words cad:: [Int])]++(convertir_monton_mierda cads)

pedazo_mierda::([Int],[[Int]])
pedazo_mierda= unsafePerformIO $ do
    s <- getContents
    let mierdas=lines(s)
        (caca:caca1:cacas)=mierdas
        numeros=map read $ words caca1 :: [Int]
    return (numeros,convertir_monton_mierda cacas)
    

consulta_caca :: MapaArbolinCaca s Int->[[Int]]->ST s [Int]
consulta_caca maputo [] = do return []
consulta_caca maputo (consul:consuls) = do
    let (inicio:final:_)=consul
    resu_mierda<-(M.lookup maputo inicio)
    resu_mierda1<-(M.lookup maputo final)
    let tu::(Arbol Int)=fromJust(resu_mierda)
        tu1::(Arbol Int)=fromJust(resu_mierda1)
        camino=camino_raiz tu
        camino1=camino_raiz tu1
        aaa_tupla=last(camino `intersect` camino1)
        (aaa,_)=aaa_tupla
        result=[aaa]
    resul_par<-(consulta_caca maputo consuls)
    return (result++resul_par)

caca_main :: [Int]
caca_main = runST$do
    let (numeros,consuls)=pedazo_mierda
    !maputo<-caca_genera_mapa::(MapaArbolins s Int)
    ass<-(caca_construye_arbol numeros Nada 0 maputo)
    cagadas<-consulta_caca maputo consuls
    return cagadas

main = do
    let !ass=caca_main
    mapM_ print ass
