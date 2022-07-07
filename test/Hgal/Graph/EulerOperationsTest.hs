{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.EulerOperationsTest where

import Control.Monad.State
import Data.Maybe
import Linear
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.EulerOperations as Euler
import Hgal.Graph.Helpers
import Hgal.Graph.Loops

import Hgal.Data.SurfaceMesh (SurfaceMesh)
import qualified Hgal.Data.SurfaceMesh as SurfaceMesh
import qualified Hgal.Data.SurfaceMeshTest as SurfaceMeshTest

u, v, w, x, y, z :: forall g v f. GraphTest g v f => v
u = vertexI @g 0
v = vertexI @g 1
w = vertexI @g 2
x = vertexI @g 3
y = vertexI @g 4
z = vertexI @g 5

f1, f2, f3 :: forall g v f. GraphTest g v f => f
f1 = faceI @g 0
f2 = faceI @g 1
f3 = faceI @g 2

class GraphTest g v f | g -> v, g -> f where
  surfaceFixture1 :: g
  surfaceFixture2 :: g
  surfaceFixture3 :: g
  vertexI :: Int -> v
  faceI :: Int -> f

instance Num a => GraphTest (SurfaceMesh.SurfaceMesh (V3 a) ()) (SurfaceMesh.Vertex) (SurfaceMesh.Face) where
  surfaceFixture1 = SurfaceMeshTest.surfaceFixture
  surfaceFixture2 = SurfaceMeshTest.surfaceFixture2
  surfaceFixture3 = SurfaceMeshTest.surfaceFixture3
  vertexI = SurfaceMesh.Vertex
  faceI = SurfaceMesh.Face


test_eulerOpertations :: IO TestTree
test_eulerOpertations = do
  specs <- concat <$> mapM testSpecs
           [ joinFaceTest (SurfaceMesh.empty () :: SurfaceMesh.SurfaceMesh (V3 Double) ())
           ]
  return $ testGroup "EulerOperations" [testGroup "SurfaceMesh static test" specs]


joinFaceTest :: forall g v h e f. GraphTest g v f
             => M.MutableHalfedgeGraph (State g) g v h e
             => M.MutableFaceGraph (State g) g v h e f
             => MutableHalfedgeGraph g v h e
             => MutableFaceGraph g v h e f
             => (Eq h, Eq f)
             => g -> Spec
joinFaceTest _ =
  describe "joinFace" $ do
    let g = surfaceFixture1 @g
        e = fromJust $ halfedgeVV g (w @g) (v @g)
        (e', g') = Euler.joinFace (setHalfedge g (f1 @g) e) e
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    context "halfedges check" $ do
      let haf = halfedgesAroundFace g' (halfedge g' (f1 @g))
      it "halfedgesAroundFace count" $ do
        length haf `shouldBe` 4
      it "halfedges have correct face" $ do
        all ((== (f1 @g)) . face g') haf `shouldBe` True
    it "deleted face is no more" $ do
      all (\x -> x == (f1 @g) || x == (f3 @g)) (faces g') `shouldBe` True
    it "vertices degree" $ do
      pending
    it "valid poylygon mesh" $ do
      pending
