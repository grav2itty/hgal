{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.EulerOperationsTest where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Either
import Data.Maybe
import Linear (V3(..))
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.EulerOperations as Euler
import Hgal.Graph.Fixtures
import Hgal.Graph.Helpers
import Hgal.Graph.Loops
import Hgal.Graph.Predicates
import Hgal.Data.SurfaceMesh (SurfaceMesh)


test_eulerOpertations :: IO TestTree
test_eulerOpertations = do
  specs <- concat <$> mapM testSpecs
           [ joinFaceTest @Double @(SurfaceMesh (V3 Double) ())
           , joinVertexInteriorTest @Double @(SurfaceMesh (V3 Double) ())
           , joinVertexExteriorTest1 @Double @(SurfaceMesh (V3 Double) ())
           , joinVertexExteriorTest2 @Double @(SurfaceMesh (V3 Double) ())
           , splitVertexTest @Double @(SurfaceMesh (V3 Double) ())
           , splitJoinVertexInverseTest @Double @(SurfaceMesh (V3 Double) ())
           ]
  return $ testGroup "EulerOperations" [testGroup "SurfaceMesh static test" specs]


testFaceFixture :: forall g v h e f. FaceGraph g v h e f
                => M.FaceGraph (State g) g v h e f
                => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                => FaceFixture g v h e f -> Spec
testFaceFixture f = do
  let g = faceFixture f
  it "valid fixture" $ do
    either id show (isValidPolygonMesh @g @v @h @e @f g) `shouldBe` "True"
    notElem (nullVertex g) (($ f) <$> [u, v, w, x, y, z]) `shouldBe` True

joinFaceTest :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
joinFaceTest = do
  f <- runIO (surfaceFixture1 @a @g @v @h @e @f @p)
  describe "joinFace" $ do
      let g = faceFixture f
          e = fromJust $ halfedgeVV g (w f) (v f)
          (e', g') = Euler.joinFace (setHalfedge g (f1 f) e) e
      testFaceFixture f
      it "edges count" $ do
        exactNumEdges g' `shouldBe` 6
      it "faces count" $ \f -> do
        exactNumFaces g' `shouldBe` 2
      context "halfedges check" $ do
        let haf = halfedgesAroundFace g' (halfedge g' (f1 f))
        it "halfedgesAroundFace count" $ do
          length haf `shouldBe` 4
        it "halfedges have correct face" $ do
          all ((== f1 f) . face g') haf `shouldBe` True
      it "deleted face is no more" $ do
        all (\x -> x == f1 f || x == f3 f) (faces g') `shouldBe` True
      it "vertices degree" $ do
        degree g' (w f) `shouldBe` 2
        degree g' (v f) `shouldBe` 3
      it "valid poylygon mesh" $ do
        either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexInteriorTest :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
joinVertexInteriorTest = do
  f <- runIO (surfaceFixture3 @a @g @v @h @e @f @p)
  describe "joinVertexInteriorTest" $ do
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (w f) (x f)
        (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 3
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (x f) `shouldBe` 4
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest1 :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
joinVertexExteriorTest1 = do
  f <- runIO (surfaceFixture3 @a @g @v @h @e @f @p)
  describe "joinVertexExteriorTest1" $ do
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (w f) (y f)
    it "fixture validity" $ do
      source g e `shouldBe` w f
      target g e `shouldBe` y f
    let (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (y f) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest2 :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
joinVertexExteriorTest2 = do
  f <- runIO (surfaceFixture3 @a @g @v @h @e @f @p)
  describe "joinVertexExteriorTest2" $ do
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (y f) (w f)
        (e', g') = Euler.joinVertex g e
    it "fixture validity" $ do
      source g e `shouldBe` y f
      target g e `shouldBe` w f
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (w f) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitVertexTest :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
splitVertexTest = do
  f <- runIO (surfaceFixture3 @a @g @v @h @e @f @p)
  describe "splitVertexTest" $ do
    let g = faceFixture f
        h1 = fromJust $ halfedgeVV g (w f) (y f)
        h2 = fromJust $ halfedgeVV g (z f) (y f)
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

splitJoinVertexInverseTest :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
splitJoinVertexInverseTest = do
  f <- runIO (surfaceFixture3 @a @g @v @h @e @f @p)
  describe "splitVertexTest" $ do
    let g = faceFixture f
        h = fromJust $ halfedgeVV g (w f) (x f)
        (_, g1) = Euler.joinVertex g h
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g1) `shouldBe` "True"
    let h1 = fromJust $ halfedgeVV g1 (z f) (x f)
        h2 = fromJust $ halfedgeVV g1 (v f) (x f)
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
