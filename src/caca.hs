{-# LANGUAGE DatatypeContexts #-}

data (Ord a, Eq a) => Arbol a = Nada | Nodo (Arbol a) (Arbol a) (Arbol a) a a
	deriving Show

insertar :: (Ord a) => Arbol a -> a -> a -> Arbol a
insertar Nada x idx = Nodo Nada Nada Nada x idx
insertar raiz@(Nodo padre t1 t2 v idx_padre) x idx = insertar_nodo raiz raiz x idx

insertar_nodo :: (Ord a) => Arbol a -> Arbol a -> a -> a -> Arbol a
insertar_nodo raiz Nada x idx = Nodo Nada raiz Nada x idx
insertar_nodo raiz actual@(Nodo padre t1 t2 v idx_padre) x idx
	| x < v = Nodo (insertar_nodo raiz padre x idx) t1 t2 v idx_padre
	| otherwise  = case t2 of Nada -> Nodo padre t1 (Nodo actual Nada Nada x idx) v idx_padre
				  t2   -> let nodo_nuevo = Nodo padre_nuevo t2 Nada x idx
					      padre_nuevo = Nodo padre t1 nodo_nuevo v idx_padre
					  in nodo_nuevo

caca_construye_arbol :: (Ord a) => [a] -> Arbol a -> a -> Arbol a
caca_construye_arbol [] arbolin idx = arbolin
caca_construye_arbol x:xs arbolin idx = caca_construye_arbol xs insertar arbolin x idx idx+1

caca_main :: a -> Int
caca_main _ = 1

main = putStrLn "Hello, World!"
