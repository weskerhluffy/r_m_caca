{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- data (Ord a, Eq a) => Arbol a = Nada | Nodo (IORef (Arbol a)) (Arbol a) (Arbol a) a a
-- 	deriving Show

data Arbol= Nodo{padre :: IORef Arbol, hi:: IORef Arbol, hdp:: IORef Arbol, val :: Int, idx :: Int} | Nada
-- data Arbol= Nodo{ padre::Arbol, hi:: Arbol, hdp:: Arbol, val :: Int, idx :: Int} | Nada

instance Show (IORef a) where
    show _ = "<ioref>"

--insertar ::  Arbol -> Int -> Int -> Arbol 
insertar Nada x idx = do
    god::(IORef Arbol) <- (newIORef Nada)
    bye <- newIORef Nada
    moon <- newIORef Nada
    caca::Arbol <- read (Nodo god bye moon x idx) :: IO Arbol
    return caca
--insertar Nada x idx = (Nodo (newIORef Nada) Nada Nada x idx)
insertar ultimo_insertado@(Nodo padre t1 t2 v idx_padre) x idx = insertar_nodo ultimo_insertado Nada x idx

--insertar_nodo :: Arbol -> Arbol -> Int -> Int -> Arbol 
insertar_nodo Nada anterior x idx = do
    men <- newIORef Nada
    return (Nodo men anterior men x idx)
--insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior x idx = do
    men <- newIORef Nada
    return (Nodo men men men 0 0)
--	| x < v = insertar_nodo padre actual x idx
--	| otherwise  = case t2 of Nada -> Nodo padre t1 (Nodo actual Nada Nada x idx) v idx_padre
--				  t2   -> let nodo_nuevo = Nodo padre_nuevo t2 Nada x idx
--					      padre_nuevo = Nodo padre t1 nodo_nuevo v idx_padre
--					  in nodo_nuevo
--
caca_construye_arbol :: [Int] -> Arbol  -> Int -> Arbol 
caca_construye_arbol [] arbolin idx = arbolin
caca_construye_arbol (x:xs) arbolin idx = caca_construye_arbol xs (insertar arbolin x idx) (idx+1)

caca_main :: a -> Int
caca_main _ = 1

main = putStrLn "Hello, World!"
