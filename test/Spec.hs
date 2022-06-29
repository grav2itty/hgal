import Test.Tasty
import Test.Tasty.Hspec

import Data.Traversable
import Linear

import Hgal.Data.SurfaceMesh


main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
           [ unconnectedVertex
           , embeddedVertex
           ]
  defaultMain (testGroup "All Tests" [testGroup "Specs" specs])

unconnectedVertex :: Spec
unconnectedVertex =
  describe "unconnected vertex" $ do
    let (f, v) = newVertex surfaceFixture (V3 10 10 10)
    it "is isolated" $ do
      isIsolated f v `shouldBe` True
    it "is on border" $ do
      isBorder f v `shouldBe` True
    it "its incident halfedge does not exist" $ do
      halfedge f v `shouldBe` nullE
    it "its degree equals 0" $ do
      degree f v `shouldBe` 0

embeddedVertex :: Spec
embeddedVertex =
  describe "embedded vertex" $ do
    let (f, v) = (surfaceFixture2, Vertex 4)
    it "is not isolated" $ do
      isIsolated f v `shouldBe` False
    it "is not on border" $ do
      isBorder f v `shouldBe` False
    it "its incident halfedge does exist" $ do
      halfedge f v /= nullE `shouldBe` True
    it "has a correct degree" $ do
      degree f v `shouldBe` 4


surfaceFixture :: Num a => SurfaceMesh (V3 a) ()
surfaceFixture = sm'
  where vs = [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0, V3 2 0 0]
        (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty ()) vs
        (sm', _) = mapAccumL newFace sm [[u, w, v], [v, w, x], [v, x, y]]

surfaceFixture2 :: Num a => SurfaceMesh (V3 a) ()
surfaceFixture2 = sm'
  where vs = [V3 0 2 0, V3 2 2 0, V3 0 0 0, V3 2 0 0, V3 1 1 0]
        (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty ()) vs
        (sm', _) = mapAccumL newFace sm [[x, v, y], [u, y, v], [u, w, y], [w, x, y]]




