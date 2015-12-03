import System.Environment
import Text.Read (readMaybe)

joinTuple :: Monad m => (m a, m b) -> m (a, b)
joinTuple (a', b') = do
          a <- a'
          b <- b'
          return (a, b)
       
addMaybes :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybes a b = uncurry (+) <$> joinTuple (a, b)

main :: IO ()
main = getArgs >>= \args ->
     print $ foldl (\x -> (x `addMaybes`) . readMaybe) (Just 0) args
