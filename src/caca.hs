-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad
import Control.Monad.ST
import Data.STRef

data Arbol a _s = Nada | Nodo (STRef _s (Arbol a _s)) (STRef _s (Arbol a _s)) (STRef _s (Arbol a _s)) a a deriving Show
-- data Arbol= Nodo{ padre::STRef s Arbol, hi:: STRef Arbol, hdp:: STRef Arbol, val :: Int, idx :: Int} | Nada deriving Show

instance Show (STRef s a) where
    show _ = "<ioref>"
--data Arbol= Nodo{padre :: Arbol, hi:: Arbol, hdp:: STRef Arbol, val :: Int, idx :: Int} | Nada

insertar :: (Ord a) => Arbol a _s-> a -> a -> Arbol a RealWorld#)
insertar Nada x idx = runST $ do
    mierda1<-newSTRef Nada
    mierda2<-newSTRef Nada
    mierda3<-newSTRef Nada
    return (Nodo (mierda1) (mierda2) (mierda3) x idx)
insertar ultimo_insertado@(Nodo padre t1 t2 v idx_padre) x idx = 
    let mierda=insertar_nodo ultimo_insertado Nada x idx
    in mierda

insertar_nodo :: (Ord a) => Arbol a s -> Arbol a s-> a -> a -> Arbol a s
insertar_nodo Nada !anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
    let ass = (Nodo Nada anterior Nada x idx)
    ass
insertar_nodo !actual@(Nodo !padre t1 t2 v idx_padre) !anterior x idx
    | x < v = insertar_nodo padre actual x idx
    | otherwise  = case t2 of Nada -> let nuevo_nodo= Nodo actual Nada Nada x idx
                                          t2_ref = unsafePerformIO(newSTRef t2)
                                          caca = writeSTRef t2_ref nuevo_nodo
                                      in nuevo_nodo
                              t2   -> let nodo_nuevo = Nodo actual t2 Nada x idx
                                          (Nodo padre_t2 _ _ _ _ ) = t2
                                          padre_t2_ref = unsafePerformIO(newSTRef padre_t2)
                                          caca = writeSTRef padre_t2_ref nodo_nuevo
                                          t2_ref = unsafePerformIO(newSTRef t2)
                                          caca1 = writeSTRef t2_ref nodo_nuevo
                                      in nodo_nuevo
insertar_nodo _ _ _ _ = Nada

--    let ass = (Nodo padre Nada Nada x idx)
--        goodbye = unsafePerformIO(newSTRef padre_ant)
--        ad = writeSTRef goodbye anterior
--    in ass

--insertar_nodo :: Arbol -> Arbol -> Int -> Int -> Arbol 
--insertar_nodo Nada anterior x idx = do
--    men <- newSTRef Nada
--    return (Nodo men anterior men x idx)
--insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
--insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior x idx = do
--    men <- newSTRef Nada
--    return (Nodo men men men 0 0)

--
caca_construye_arbol :: (Ord a, Num a) => [a] -> Arbol a s -> a -> Arbol a s
caca_construye_arbol [] arbolin idx = arbolin
caca_construye_arbol (x:xs) !arbolin idx = caca_construye_arbol xs (insertar arbolin x idx) (idx+1)

--caca_main :: a -> Int
--caca_main _ = 1
main = do
    let ass=(caca_construye_arbol [3,2,1] Nada 0)
    print ("ass "++(show ass))
