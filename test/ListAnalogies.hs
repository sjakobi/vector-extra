import qualified Data.Vector.Generic.Extra as E

import Data.Either
import Data.Maybe
import Data.Vector
import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain analogies

analogousTo :: Eq b => (Vector a -> Vector b) -> ([a] -> [b]) -> [a] -> Bool
analogousTo vf lf = vf `analogousTo'` lf `via` (fromList, toList)

analogousTo' :: a -> b -> (a, b)
analogousTo' vf lf = (vf, lf)

via :: (Eq d) => (a -> b, c -> d) -> (c -> a, b -> d) -> (c -> Bool)
via (vf, lf) (f, g) = \c -> (g . vf . f) c == lf c

analogies :: TestTree
analogies = testGroup "Analogies to functions on []" [maybeFuns, eitherFuns]

maybeFuns :: TestTree
maybeFuns = testGroup "Maybe utilities"
  [ SC.testProperty "mapMaybe" $ SC.changeDepth (min 2) $
      \f -> E.mapMaybe f `analogousTo` mapMaybe (f :: Int -> Maybe Char)
  , SC.testProperty "catMaybes" $
      E.catMaybes `analogousTo` (catMaybes :: [Maybe Bool] -> [Bool])
  , SC.testProperty "vectorToMaybe" $
      E.vectorToMaybe `analogousTo'` (listToMaybe :: [Int] -> Maybe Int) `via`
        (fromList, id)
  , SC.testProperty "maybeToVector" $
      E.maybeToVector `analogousTo'` (maybeToList :: Maybe () -> [()]) `via`
        (id, toList)
  ]

eitherFuns :: TestTree
eitherFuns = testGroup "Either utilities"
  [ SC.testProperty "lefts" $
      E.lefts `analogousTo` (lefts :: [Either String Int] -> [String])
  , SC.testProperty "rights" $
      E.rights `analogousTo` (rights :: [Either String Int] -> [Int])
  , SC.testProperty "partitionEithers" $
      E.partitionEithers `analogousTo'`
        (partitionEithers :: [Either Int Int] -> ([Int], [Int])) `via`
        (fromList, \(va, vb) -> (toList va, toList vb))
  ]
