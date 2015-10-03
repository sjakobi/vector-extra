module Data.Vector.Fusion.Stream.Monadic.Extra where

import Data.Vector.Fusion.Stream.Size (toMax)
import Data.Vector.Fusion.Stream.Monadic (Stream(..),Step(..))

mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = mapMaybeM (return . f)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f (Stream step s n) = Stream step' s (toMax n)
  where
    step' t = do r <- step t
                 case r of
                   Yield a t' -> do mb <- f a
                                    return $ case mb of
                                      Just b -> Yield b t'
                                      Nothing -> Skip t'
                   Skip t' -> return (Skip t')
                   Done -> return Done
