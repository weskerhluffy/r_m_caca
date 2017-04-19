{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Arbol a = Nada | Nodo (Arbol a) (Arbol a) (Arbol a) a a
    deriving Show

--data Arbol= Nodo{padre :: Arbol, hi:: Arbol, hdp:: IORef Arbol, val :: Int, idx :: Int} | Nada
-- data Arbol= Nodo{ padre::Arbol, hi:: Arbol, hdp:: Arbol, val :: Int, idx :: Int} | Nada

instance Show (IORef a) where
    show _ = "<ioref>"

insertar :: (Ord a) => Arbol a -> a -> a -> Arbol a
insertar Nada x idx = let caca = (Nodo Nada Nada Nada x idx)
                      in caca
insertar ultimo_insertado@(Nodo padre t1 t2 v idx_padre) x idx = 
    let mierda=insertar_nodo ultimo_insertado Nada x idx
    in mierda

insertar_nodo :: (Ord a) => Arbol a -> Arbol a -> a -> a -> Arbol a
insertar_nodo Nada anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx = do
    let ass = (Nodo Nada anterior Nada x idx)
    ass
insertar_nodo actual@(Nodo padre t1 t2 v idx_padre) anterior@(Nodo padre_ant hi_ant hdp_ant v_ant idx_ant) x idx
	| x < v = insertar_nodo padre actual x idx
	| otherwise  = case t2 of Nada -> let nuevo_nodo= Nodo padre Nada Nada v idx
	                                      t2_ref = unsafePerformIO(newIORef t2)
					      caca = writeIORef t2_ref nuevo_nodo
					  in nuevo_nodo
				  t2   -> let nodo_nuevo = Nodo padre t2 Nada x idx
					      (Nodo padre_t2 _ _ _ _ ) = t2
					      padre_t2_ref = unsafePerformIO(newIORef padre_t2)
					      caca = writeIORef padre_t2_ref nodo_nuevo
	                                      t2_ref = unsafePerformIO(newIORef t2)
					      caca1 = writeIORef t2_ref nodo_nuevo
					  in nodo_nuevo

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
--caca_construye_arbol :: [Int] -> Arbol  -> Int -> Arbol 
--caca_construye_arbol [] arbolin idx = arbolin
--caca_construye_arbol (x:xs) arbolin idx = caca_construye_arbol xs (insertar arbolin x idx) (idx+1)

--caca_main :: a -> Int
--caca_main _ = 1

main = putStrLn "Hello, World!"
