-- XXX: http://stackoverflow.com/questions/7880555/st-monad-code-smell
import Control.Monad
import Control.Monad.ST
import Data.STRef

data STRefPair s a b = STRefPair { left :: STRef s a, right :: STRef s b }

mkStRefPair :: a -> b -> ST s (STRefPair s a b)
mkStRefPair a b = do
    a' <- newSTRef a
    b' <- newSTRef b
    return $ STRefPair a' b'

derp :: (Num a, Num b) => STRefPair s a b -> ST s ()
derp p = do
    modifySTRef (left p) (\x -> x + 1)
    modifySTRef (right p) (\x -> x - 1)

herp :: (Num a, Num b) => (a, b)
herp = runST $ do
    p <- mkStRefPair 0 0
    replicateM_ 10 $ derp p
    a <- readSTRef $ left p
    b <- readSTRef $ right p
    return (a, b)

main = print herp -- should print (10, -10)
