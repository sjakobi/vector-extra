{-# LANGUAGE FlexibleContexts #-}

module Data.Vector.Generic.Extra where

import qualified Data.Vector.Fusion.Stream.Monadic.Extra as MStream

import Data.Either (isLeft,isRight)
import Data.Either.Extra (fromLeft,fromRight)
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G

mapMaybe :: (Vector v a, Vector v b) => (a -> Maybe b) -> v a -> v b
mapMaybe f = G.unstream . Stream.inplace (MStream.mapMaybe f) . G.stream

catMaybes :: (Vector v (Maybe a), Vector v a) => v (Maybe a) -> v a
catMaybes = mapMaybe id

vectorToMaybe :: (Vector v a) => v a -> Maybe a
vectorToMaybe v
  | G.null v = Nothing
  | otherwise = Just (G.head v)

maybeToVector :: (Vector v a) => Maybe a -> v a
maybeToVector mx =
  case mx of
    Just x -> G.singleton x
    Nothing -> G.empty

lefts :: (Vector v (Either a b), Vector v a) => v (Either a b) -> v a
lefts = G.map (\(Left x) -> x) . G.filter isLeft

rights :: (Vector v (Either a b), Vector v b) => v (Either a b) -> v b
rights = G.map (\(Right x) -> x) . G.filter isRight

partitionEithers :: (Vector v (Either a b), Vector v a, Vector v b)
                 => v (Either a b) -> (v a, v b)
partitionEithers =
  (\(ls, rs) -> (G.map fromLeft ls, G.map fromRight rs)) . G.partition isLeft
