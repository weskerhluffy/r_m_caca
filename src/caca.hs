-- XXX: https://mail.haskell.org/pipermail/beginners/2012-April/009743.html
-- XXX: https://en.wikibooks.org/wiki/Haskell/Mutable_objects
-- XXX: http://wiki.c2.com/?HaskellExampleForMutabilityOnObjects
-- XXX: http://stackoverflow.com/questions/4418017/haskell-new-instance-declaration-for-show
-- XXX: http://blog.jakubarnold.cz/2014/07/20/mutable-state-in-haskell.html
-- XXX: https://news.ycombinator.com/item?id=1831403
-- XXX: http://stackoverflow.com/questions/8332307/show-for-io-types
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
    M.insert maputo x_shit ultimo 
    putamierda<-caca_construye_arbol xs ultimo (idx+1) maputo
    return putamierda

encuentra_raiz :: (Ord a) => Arbol a -> Arbol a
encuentra_raiz Nada = Nada
encuentra_raiz !actual@(Nodo padre _ _ x idx) = let padre_cont = unsafePerformIO(readIORef(padre))
                                                in case padre_cont of Nada -> actual
                                                                      padre_cont ->encuentra_raiz padre_cont

inorder :: (Ord c, Num c) => Arbol c -> c -> [(c,c,c)]
inorder Nada _ = []
inorder (Nodo _ hi hdp x idx) derp = (inorder (unsafePerformIO(readIORef(hi))) (derp+1)) ++ [(x,idx,derp)] ++ (inorder (unsafePerformIO(readIORef(hdp))) (derp+1))

caca_main :: ST s (Bool)
caca_main = do
    maputo<-caca_genera_mapa::(MapaArbolins s Int)
    ass<-(caca_construye_arbol [50,8,7,45,3,56,3,335,4232,24] Nada 0 maputo)
    let raiz=encuentra_raiz ass
        !caca=print ("ass "++(show ass)++" i la raiz "++(show raiz))
        !caca1= print ("fuc "++ (show (inorder raiz 0)))
    resu_mierda<-(M.lookup 0 maputo)
    tu::(Arbol Int)<-fromJust(resu_mierda)
    let !caca2=print ("wes una "++(show tu))
    return True


main = do
    print "unas voi"
    caca_main
