import Data.Vector.Generic.Extra

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "mapMaybe" $ do
    context "when provided with \"const Nothing\"" $ do
      context "and a regular Vector" $
        it "returns \"empty\"" $ property $
          \xs -> let f :: Int -> Maybe Char
                     f = const Nothing
                 in mapMaybe f xs == V.empty
      context "and an unboxed Vector" $
        it "returns \"empty\"" $ property $
          \xs -> let f :: Int -> Maybe Char
                     f = const Nothing
                 in mapMaybe f xs == U.empty
    context "when provided with \"\\x -> if odd x then Nothing else Just x\"" $
      it "returns only the even elements" $ property $
        \xs -> let f :: Word -> Maybe Word
                   f x =
                     if odd x
                       then Nothing
                       else Just x
                   xs' = mapMaybe f xs
               in U.all even xs'

  describe "catMaybes" $ do
    it "works" $ property $
      \xs -> let xs' = V.map (const Nothing) xs V.++ V.map Just xs
             in catMaybes xs' == (xs :: V.Vector Char)

  describe "vectorToMaybe" $ do
      it "is inverse to \"singleton . fromJust\"" $ property $
        \x -> vectorToMaybe (U.singleton x) == Just (x :: ())
      context "when provided with an empty Vector" $
        it "returns Nothing" $
          vectorToMaybe V.empty == (Nothing :: Maybe Bool)

  describe "maybeToVector" $ do
    context "when provided with Nothing" $
      it "returns an empty Vector" $
        maybeToVector Nothing == (U.empty :: U.Vector Double)
    context "when provided with a Just a" $
      it "returns a singleton containing the a" $ property $
        \x -> maybeToVector (Just x) == V.singleton (x :: Int)

  describe "lefts" $ do
    it "is inverse to \"map Left\"" $ property $
        \x -> (lefts . V.map Left) x == (x :: V.Vector Word)

  describe "rights" $ do
    it "is inverse to \"map Right\"" $ property $
      \x -> (rights . V.map Right) x == (x :: V.Vector String)

  describe "partitionEithers" $ do
    it "plays nice" $ property $
      \(xs, ys) -> let ls = V.map Left xs
                       rs = V.map Right ys
                   in partitionEithers (rs V.++ ls) ==
                        (xs :: V.Vector Bool, ys :: V.Vector Integer)
    it "real nice" $ property $
      \xs -> let (ls, rs) = partitionEithers xs
             in V.length ls + V.length rs == V.length (xs :: V.Vector (Either Float Double))

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = fmap V.fromList . shrink . V.toList

instance (U.Unbox a, Arbitrary a) => Arbitrary (U.Vector a) where
  arbitrary = U.fromList <$> arbitrary
  shrink = fmap U.fromList . shrink . U.toList
