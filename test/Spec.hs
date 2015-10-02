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
        it "returns \"empty\"" $
          mapMaybe (const Nothing) (V.fromList ([1..10] :: [Int]))
            `shouldBe` (V.empty :: V.Vector Char)
      context "and an unboxed Vector" $
        it "returns \"empty\"" $
          mapMaybe (const Nothing) (U.fromList ([1..10] :: [Int]))
            `shouldBe` (U.empty :: U.Vector Char)
    context "when provided with \"\\x -> if odd x then Nothing else Just x\"" $
      it "returns only the even elements" $
        mapMaybe (\x -> if odd x then Nothing else Just x) (U.fromList [1..10])
          `shouldBe` (U.fromList ([2,4..10] :: [Word]))

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
