module Data.Vector.Generic.Extra where

import qualified Data.Vector.Fusion.Stream.Monadic.Extra as Stream

import Data.Vector.Fusion.Stream
import Data.Vector.Generic

mapMaybe :: (Vector v a, Vector v b) => (a -> Maybe b) -> v a -> v b
mapMaybe f = unstream . inplace (Stream.mapMaybe f) . stream
