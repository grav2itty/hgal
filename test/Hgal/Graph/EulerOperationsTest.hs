{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.EulerOperationsTest where

import Control.Lens
import Control.Monad.State
import Data.Either
import Data.Maybe
import Linear
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.EulerOperations as Euler
import Hgal.Graph.Helpers
import Hgal.Graph.Loops
import Hgal.Graph.Predicates

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

instance Num a => GraphTest (SurfaceMesh.SurfaceMesh (V3 a) ()) SurfaceMesh.Vertex SurfaceMesh.Face where
  surfaceFixture1 = SurfaceMeshTest.surfaceFixture
  surfaceFixture2 = SurfaceMeshTest.surfaceFixture2
  surfaceFixture3 = SurfaceMeshTest.surfaceFixture3
  vertexI = SurfaceMesh.Vertex
  faceI = SurfaceMesh.Face


test_eulerOpertations :: IO TestTree
test_eulerOpertations = do
  let sm = SurfaceMesh.empty () :: SurfaceMesh.SurfaceMesh (V3 Double) ()
  specs <- concat <$> mapM testSpecs
           [ joinFaceTest sm
           , joinVertexInteriorTest sm
           , joinVertexExteriorTest1 sm
           , joinVertexExteriorTest2 sm
           , splitVertexTest sm
           , splitJoinVertexInverseTest sm
           ]
  return $ testGroup "EulerOperations" [testGroup "SurfaceMesh static test" specs]


joinFaceTest :: forall g v h e f. GraphTest g v f
             => M.MutableHalfedgeGraph (State g) g v h e
             => M.MutableFaceGraph (State g) g v h e f
             => MutableHalfedgeGraph g v h e
             => MutableFaceGraph g v h e f
             => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
             => g -> Spec
joinFaceTest _ =
  describe "joinFace" $ do
    let g = surfaceFixture1 @g
        e = fromJust $ halfedgeVV g (w @g) (v @g)
        (e', g') = Euler.joinFace (setHalfedge g (f1 @g) e) e
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    context "halfedges check" $ do
      let haf = halfedgesAroundFace g' (halfedge g' (f1 @g))
      it "halfedgesAroundFace count" $ do
        length haf `shouldBe` 4
      it "halfedges have correct face" $ do
        all ((== (f1 @g)) . face g') haf `shouldBe` True
    it "deleted face is no more" $ do
      all (\x -> x == (f1 @g) || x == (f3 @g)) (faces g') `shouldBe` True
    it "vertices degree" $ do
      degree g' (w @g) `shouldBe` 2
      degree g' (v @g) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexInteriorTest :: forall g v h e f. GraphTest g v f
                       => M.MutableHalfedgeGraph (State g) g v h e
                       => M.MutableFaceGraph (State g) g v h e f
                       => MutableHalfedgeGraph g v h e
                       => MutableFaceGraph g v h e f
                       => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                       => g -> Spec
joinVertexInteriorTest g =
  describe "joinVertexInteriorTest" $ do
    let g = surfaceFixture3 @g
        e = fromJust $ halfedgeVV g (w @g) (x @g)
        (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 @g))) `shouldBe` 3
      length (halfedgesAroundFace g' (halfedge g' (f2 @g))) `shouldBe` 3
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (x @g) `shouldBe` 4
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest1 :: forall g v h e f. GraphTest g v f
                        => M.MutableHalfedgeGraph (State g) g v h e
                        => M.MutableFaceGraph (State g) g v h e f
                        => MutableHalfedgeGraph g v h e
                        => MutableFaceGraph g v h e f
                        => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                        => g -> Spec
joinVertexExteriorTest1 g =
  describe "joinVertexExteriorTest1" $ do
    let g = surfaceFixture3 @g
        e = fromJust $ halfedgeVV g (w @g) (y @g)
    it "fixture validity" $ do
      source g e `shouldBe` (w @g)
      target g e `shouldBe` (y @g)
    let (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 @g))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 @g))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (y @g) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest2 :: forall g v h e f. GraphTest g v f
                        => M.MutableHalfedgeGraph (State g) g v h e
                        => M.MutableFaceGraph (State g) g v h e f
                        => MutableHalfedgeGraph g v h e
                        => MutableFaceGraph g v h e f
                        => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                        => g -> Spec
joinVertexExteriorTest2 g =
  describe "joinVertexExteriorTest2" $ do
    let g = surfaceFixture3 @g
        e = fromJust $ halfedgeVV g (y @g) (w @g)
        (e', g') = Euler.joinVertex g e
    it "fixture validity" $ do
      source g e `shouldBe` (y @g)
      target g e `shouldBe` (w @g)
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 @g))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 @g))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (w @g) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitVertexTest :: forall g v h e f. GraphTest g v f
                => M.MutableHalfedgeGraph (State g) g v h e
                => M.MutableFaceGraph (State g) g v h e f
                => MutableHalfedgeGraph g v h e
                => MutableFaceGraph g v h e f
                => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                => g -> Spec
splitVertexTest g =
  describe "splitVertexTest" $ do
    let g = surfaceFixture3 @g
        h1 = fromJust $ halfedgeVV g (w @g) (y @g)
        h2 = fromJust $ halfedgeVV g (z @g) (y @g)
    it "fixture validity" $ do
      face g h2 `shouldBe` nullFace g
    let (e', g') = Euler.splitVertex g h1 h2
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 7
    it "halfedges count" $ do
      length (halfedgesAroundFace g' h1) `shouldBe` 5
      length (halfedgesAroundFace g' h2) `shouldBe` 7
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 8
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitJoinVertexInverseTest :: forall g v h e f. GraphTest g v f
                           => M.MutableHalfedgeGraph (State g) g v h e
                           => M.MutableFaceGraph (State g) g v h e f
                           => MutableHalfedgeGraph g v h e
                           => MutableFaceGraph g v h e f
                           => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                           => g -> Spec
splitJoinVertexInverseTest g =
  describe "splitVertexTest" $ do
    let g = surfaceFixture3 @g
        h = fromJust $ halfedgeVV g (w @g) (x @g)
        (_, g1) = Euler.joinVertex g h
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g1) `shouldBe` "True"
    let h1 = fromJust $ halfedgeVV g1 (z @g) (x @g)
        h2 = fromJust $ halfedgeVV g1 (v @g) (x @g)
        (e, g2) = Euler.splitVertex g1 h1 h2
        (_, g3) = Euler.joinVertex g2 e
    it "vertex count" $ do
      exactNumVertices g3 `shouldBe` 5
    it "halfedges count" $ do
      exactNumHalfedges g3 `shouldBe` 12
      length (halfedgesAroundFace g3 h1) `shouldBe` 3
      length (halfedgesAroundFace g3 h2) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g3 `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g3 `shouldBe` 2
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g3) `shouldBe` "True"
