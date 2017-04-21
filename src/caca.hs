-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE TypeOperators #-}
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Arbol a = Nada | Nodo (IORef (Arbol a)) (IORef (Arbol a)) (IORef (Arbol a)) a a deriving Show
-- data Arbol= Nodo{ padre::IORef s Arbol, hi:: IORef Arbol, hdp:: IORef Arbol, val :: Int, idx :: Int} | Nada deriving Show

instance Show (IORef a) where
    show _ = "<ioref>"
--data Arbol= Nodo{padre :: Arbol, hi:: Arbol, hdp:: IORef Arbol, val :: Int, idx :: Int} | Nada

insertar :: (Ord a) => Arbol a-> a -> a -> IO (Arbol a)
insertar Nada x idx = do
    mierda1<-newIORef Nada
    mierda2<-newIORef Nada
    mierda3<-newIORef Nada
    return (Nodo (mierda1) (mierda2) (mierda3) x idx)
insertar ultimo_insertado@(Nodo padre t1 t2 v idx_padre) x idx = do
--    let mierda=unsafePerformIO((insertar_nodo ultimo_insertado Nada x idx))
    mierda<-((insertar_nodo ultimo_insertado Nada x idx))
    return mierda

insertar_nodo :: (Ord a) => Arbol a -> Arbol a-> a -> a -> IO (Arbol a)
insertar_nodo Nada !anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
    mierda1<-newIORef Nada
    mierda2<-newIORef Nada
    mierda3<-newIORef Nada
    mierda4<-newIORef anterior
    let ass = (Nodo mierda1 mierda4 mierda2 x idx)
    return ass
insertar_nodo !actual@(Nodo !padre t1 t2 v idx_padre) !anterior x idx
    | x < v = do padre_ref<-readIORef(padre)
                 insertar_nodo padre_ref actual x idx
    | otherwise  = do t2_cont<-readIORef(t2)
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
insertar_nodo _ _ _ _ = do return Nada

--    let ass = (Nodo padre Nada Nada x idx)
--        goodbye = unsafePerformIO(newIORef padre_ant)
--        ad = writeIORef goodbye anterior
--    in ass

--insertar_nodo :: Arbol -> Arbol -> Int -> Int -> Arbol 
--insertar_nodo Nada anterior x idx = do
--    men <- newIORef Nada
--    return (Nodo men anterior men x idx)
--insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
--insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior x idx = do
--    men <- newIORef Nada
--    return (Nodo men men men 0 0)

--
caca_construye_arbol :: (Ord a, Num a) => [a] -> Arbol a -> a -> Arbol a
caca_construye_arbol [] arbolin idx = arbolin
caca_construye_arbol (x:xs) !arbolin idx = caca_construye_arbol xs (insertar arbolin x idx) (idx+1)

--caca_main :: a -> Int
--caca_main _ = 1
main = do
    let ass=(caca_construye_arbol [3,2,1] Nada 0)
    print ("ass "++(show ass))
