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
import Data.List

data Tree a = Nil | Node Int (Tree Int) (Tree Int) deriving Eq

instance Show (Tree a) where
    show Nil= ""
    show (Node val hi hdp) =" "++(if hi==Nil then "" else show hi)++":" ++ (show val)++":"++(if hi==Nil then "" else show hdp)++" "

                                                                      
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
    

caca_construye_arbol_segmentos::(Ord Int) => [Int]->Int->Int->Int->Tree Int
caca_construye_arbol_segmentos nums num_nums limite_izq limite_der
    | limite_izq == limite_der = let valor = if limite_izq < num_nums then (nums!!limite_izq) else 100000
                                 in (Node valor Nil Nil)
    | otherwise = let limite_med=limite_izq+((limite_der-limite_izq) `quot` 2)
                      hi=(caca_construye_arbol_segmentos nums num_nums limite_izq limite_med)
                      hdp=(caca_construye_arbol_segmentos nums num_nums (limite_med+1) limite_der)
                      (Node hi_val _ _) = hi
                      (Node hdp_val _ _) = hdp
                      caca_val=(min hi_val hdp_val)
                  in (Node caca_val hi hdp)

caca_main :: [Int]
caca_main = unsafePerformIO $ do
    let (numeros,consuls)=pedazo_mierda
        num_nums=length numeros
        ass=(caca_construye_arbol_segmentos numeros num_nums 0 (num_nums-1))
    print("rikolina "++(show ass))
--        cagadas=[1,2]
--    cagadas<-consulta_caca maputo consuls 
    return [1]

main = do
    let !ass=caca_main
    mapM_ print ass
