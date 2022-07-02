module Hgal.Graph.GeneratorsM where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import Linear hiding (point)

import Hgal.Graph.ClassM
import Hgal.Data.PropertyM
import qualified Hgal.Graph.EulerOperationsM as Euler


makeTriangle :: MutableHalfedgeGraph m g
             => MutableFaceGraph m g
             => PointGraph g
             => Property m g (Point g) p
             => g
             -> p -> p -> p
             -> m (Halfedge g)
makeTriangle g p0 p1 p2 = do
  v0 <- addVertex g
  v1 <- addVertex g
  v2 <- addVertex g
  h0 <- halfedgeE g =<< addEdge g
  h1 <- halfedgeE g =<< addEdge g
  h2 <- halfedgeE g =<< addEdge g
  setNext g h0 h1
  setNext g h1 h2
  setNext g h2 h0
  setTarget g h0 v1
  setTarget g h1 v2
  setTarget g h2 v0
  setHalfedgeV g v1 h0
  setHalfedgeV g v2 h1
  setHalfedgeV g v0 h2
  f <- addFace g
  setFace g h0 f
  setFace g h1 f
  setFace g h2 f
  setHalfedgeF g f h0
  h0' <- opposite g h0
  h1' <- opposite g h1
  h2' <- opposite g h2
  setNext g h0' h2'
  setNext g h2' h1'
  setNext g h1' h0'
  setTarget g h0' v0
  setTarget g h1' v1
  setTarget g h2' v2
  nullF <- nullFace g
  setFace g h0' nullF
  setFace g h1' nullF
  setFace g h2' nullF

  replaceProperty g (point g v0) p0
  replaceProperty g (point g v1) p1
  replaceProperty g (point g v2) p2

  opposite g h2'

makeQuad :: MutableHalfedgeGraph m g
         => MutableFaceGraph m g
         => PointGraph g
         => Property m g (Point g) p
         => g
         -> p -> p -> p -> p
         -> m (Halfedge g)
makeQuad g p0 p1 p2 p3 = do
  v0 <- addVertex g
  v1 <- addVertex g
  v2 <- addVertex g
  v3 <- addVertex g
  replaceProperty g (point g v0) p0
  replaceProperty g (point g v1) p1
  replaceProperty g (point g v2) p2
  replaceProperty g (point g v3) p3
  formQuad g v0 v2 v2 v3

formQuad :: MutableHalfedgeGraph m g
         => MutableFaceGraph m g
         => g
         -> Vertex g -> Vertex g -> Vertex g -> Vertex g
         -> m (Halfedge g)
formQuad g v0 v1 v2 v3 = do
  h0 <- halfedgeE g =<< addEdge g
  h1 <- halfedgeE g =<< addEdge g
  h2 <- halfedgeE g =<< addEdge g
  h3 <- halfedgeE g =<< addEdge g
  setNext g h0 h1
  setNext g h1 h2
  setNext g h2 h3
  setNext g h3 h0
  setTarget g h0 v1
  setTarget g h1 v2
  setTarget g h2 v3
  setTarget g h3 v0
  setHalfedgeV g v1 h0
  setHalfedgeV g v2 h1
  setHalfedgeV g v3 h2
  setHalfedgeV g v0 h3
  f <- addFace g
  setFace g h0 f
  setFace g h1 f
  setFace g h2 f
  setFace g h3 f
  setHalfedgeF g f h0
  h0' <- opposite g h0
  h1' <- opposite g h1
  h2' <- opposite g h2
  h3' <- opposite g h3
  setNext g h0' h3'
  setNext g h3' h2'
  setNext g h2' h1'
  setNext g h1' h0'
  setTarget g h0' v0
  setTarget g h1' v1
  setTarget g h2' v2
  setTarget g h3' v3
  nullF <- nullFace g
  setFace g h0' nullF
  setFace g h1' nullF
  setFace g h2' nullF
  setFace g h3' nullF
  opposite g h3'

makeHexahedron :: Eq (Halfedge g)
               => MutableHalfedgeGraph m g
               => MutableFaceGraph m g
               => PointGraph g
               => Property m g (Point g) p
               => g
               -> p -> p -> p -> p
               -> p -> p -> p -> p
               -> m (Halfedge g)
makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7 = do
  vs <- replicateM 8 (addVertex g)
  let [v0, v1, v2, v3, v4, v5, v6, v7] = vs
  ht <- formQuad g v4 v5 v6 v7
  hb <- prev g =<< formQuad g v0 v3 v2 v1
  let
    worker (ht', hb') _ = do
      h <- halfedgeE g =<< addEdge g
      setTarget g h =<< target g hb'
      setNext g h =<< opposite g hb'
      (setNext g ?? h) =<< (opposite g <=< prev g) ht'
      h' <- opposite g h
      setTarget g h' =<< (source g <=< prev g) ht'
      setNext g h' =<< (opposite g <=< next g <=< next g) ht'
      (setNext g ?? h) =<< (opposite g <=< next g) hb'
      liftM2 (,) (prev g ht) (next g hb)
    worker2 hb' _ = do
      Euler.fillHole g =<< opposite g hb'
      next g hb'
  (_, hb') <- foldM worker (ht, hb) [0..3]
  hb'' <- foldM worker2 hb' [0..3]

  mapM_ (uncurry $ replaceProperty g) (zip (point g <$> vs) [p0, p1, p2, p3, p4, p5, p6, p7])

  (next g <=< next g) hb''

makeRegularPrism :: Floating a
                 => R3 p
                 => Eq (Halfedge g)
                 => Eq (Face g)
                 => Ord (Vertex g)
                 => MutableHalfedgeGraph m g
                 => MutableFaceGraph m g
                 => PointGraph g
                 => Property m g (Point g) (p a)
                 => g
                 -> Int
                 -> p a
                 -> a -> a
                 -> Bool
                 -> m (Halfedge g)
makeRegularPrism g n center height radius isClosed = do
  let step = assert (n >= 3) $
             2 * pi / fromIntegral n

  vs <- V.replicateM (2*n) (addVertex g)

  forM_ [0..n-1] $ \i -> do
    let i' = fromIntegral i
    let p1 = center & _x +~ (radius * cos (i' * step))
                    & _z -~ (radius * sin (i' * step))
    let p2 = p1 & _y +~ height
    replaceProperty g (point g $ vs ! (i + n)) p1
    replaceProperty g (point g $ vs ! i) p2

  forM_ [0..n-1] $ \i -> do
    let ii = mod (i+1) n
    Euler.addFace g [vs ! ii, vs ! i, vs ! (ii + n)]
    Euler.addFace g [vs ! (ii + n), vs ! i, vs ! (i + n)]

  when isClosed $ do
    top <- addVertex g
    bot <- addVertex g
    replaceProperty g (point g top) (_y +~ height $ center)
    replaceProperty g (point g bot) center

    forM_ [0..n-1] $ \i -> do
      let ii = mod (i+1) n
      Euler.addFace g [vs ! i, vs ! ii, top]
      Euler.addFace g [bot, vs ! (ii + n), vs ! (i + n)]

  fromJust <$> halfedgeVV g (vs ! 0) (vs ! 1)
