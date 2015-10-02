import Data.Vector.Generic.Extra

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

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
