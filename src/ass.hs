import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
 -- /Library/Haskell/bin/uninstall-hs

data Arbol a = Nada | Nodo (Arbol a) (Arbol a) (Arbol a) a a
        deriving Show
-- data Point = XY {x :: IORef Int, y :: IORef Int} | CACA
data Point = XY {x :: IORef Int, y :: IORef Int, puto :: IORef Point} | CACA
-- data Point = XY {x :: Int, y :: Int, puto :: Point} | CACA deriving Show

setupGUI :: IORef Int -> IO ()
setupGUI counter = do
    print "video killj"
    -- However much other GUI preparation code we need.
    (modifyIORef counter (+1))

newPoint (x, y, f) = do xRef <- newIORef x
                        yRef <- newIORef y
                        fRef <- newIORef f
                        return (XY xRef yRef fRef)

--creaPuto :: Int -> Point
--creaPuto caca = let xRef = newIORef (10::Int)
--                    yRef = newIORef (20::Int)
--                in (XY xRef yRef)
--    (XY 10  20  CACA)
--    (XY (newIORef 10 ) (newIORef 20)  CACA)
printPoint (XY x y f) = do x' <- readIORef x
                           y' <- readIORef y
                           f' <- readIORef f
                           print (x', y')

movePoint (deltaX, deltaY, mierda) (XY x y f)  = do modifyIORef x (+ deltaX)
                                                    modifyIORef y (+ deltaY)

poncaca (mierda) (XY x y f)  = do writeIORef f mierda

cagada :: Point -> String
cagada (CACA) = "a la mierda nada"
cagada (XY x y f) = let x' = readIORef x
                        y' = readIORef y
                        xcaca = (unsafePerformIO  x')
                        ycaca = (unsafePerformIO  y')
                    in (show xcaca)++" "++(show ycaca)

fuck :: Point -> IO()
fuck (CACA) = print "q berga"
fuck shit@(XY x y f) = do print "ingrata"
                          print (cagada shit)
                          let a=1
                          fuck (unsafePerformIO (readIORef f))

main :: IO ()
main = do
    -- etc.
    counter <- newIORef (0 :: Int)
    readIORef counter >>= print
    setupGUI counter
    readIORef counter >>= print
    puto1<- newPoint (10,201,CACA)
    readIORef counter >>= print
    printPoint puto1
    movePoint(100,200,CACA) puto1
    printPoint puto1
    print (cagada puto1)
    fuck puto1
    puto2<- newPoint (455,677,CACA)
    print "a ver"
    poncaca puto2 puto1
    fuck puto1
    print "putada"
    
    -- Then just use the counter value wherever necessary.
