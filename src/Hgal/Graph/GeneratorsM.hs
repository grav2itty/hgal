module Hgal.Graph.GeneratorsM where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import Linear hiding (point)

import Hgal.Graph.ClassM
import Hgal.Data.PropertyM
import qualified Hgal.Graph.EulerOperationsM as Euler


makeTriangle :: MutableHalfedgeGraph m g v h e
             => MutableFaceGraph m g v h e f
             => PointGraph g v k
             => Property m g k p
             => g -> p -> p -> p -> m h
makeTriangle g p0 p1 p2 = do
  v0 <- addVertex g
  v1 <- addVertex g
  v2 <- addVertex g
  h0 <- halfedge =<< addEdge g
  h1 <- halfedge =<< addEdge g
  h2 <- halfedge =<< addEdge g
  setNext h0 h1
  setNext h1 h2
  setNext h2 h0
  setTarget h0 v1
  setTarget h1 v2
  setTarget  h2 v0
  setHalfedge v1 h0
  setHalfedge v2 h1
  setHalfedge v0 h2
  f <- addFace g
  setFace h0 f
  setFace h1 f
  setFace h2 f
  setHalfedge f h0
  h0' <- opposite h0
  h1' <- opposite h1
  h2' <- opposite h2
  setNext h0' h2'
  setNext h2' h1'
  setNext h1' h0'
  setTarget h0' v0
  setTarget h1' v1
  setTarget h2' v2
  nullF <- nullFace g
  setFace h0' nullF
  setFace h1' nullF
  setFace h2' nullF

  replaceProperty g (point g v0) p0
  replaceProperty g (point g v1) p1
  replaceProperty g (point g v2) p2

  opposite h2'

makeQuad :: MutableHalfedgeGraph m g v h e
         => MutableFaceGraph m g v h e f
         => PointGraph g v k
         => Property m g k p
         => g -> p -> p -> p -> p -> m h
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

formQuad :: MutableHalfedgeGraph m g v h e
         => MutableFaceGraph m g v h e f
         => g -> v -> v -> v -> v -> m h
formQuad g v0 v1 v2 v3 = do
  h0 <- halfedge =<< addEdge g
  h1 <- halfedge =<< addEdge g
  h2 <- halfedge =<< addEdge g
  h3 <- halfedge =<< addEdge g
  setNext h0 h1
  setNext h1 h2
  setNext h2 h3
  setNext h3 h0
  setTarget h0 v1
  setTarget h1 v2
  setTarget h2 v3
  setTarget h3 v0
  setHalfedge v1 h0
  setHalfedge v2 h1
  setHalfedge v3 h2
  setHalfedge v0 h3
  f <- addFace g
  setFace h0 f
  setFace h1 f
  setFace h2 f
  setFace h3 f
  setHalfedge f h0
  h0' <- opposite h0
  h1' <- opposite h1
  h2' <- opposite h2
  h3' <- opposite h3
  setNext h0' h3'
  setNext h3' h2'
  setNext h2' h1'
  setNext h1' h0'
  setTarget h0' v0
  setTarget h1' v1
  setTarget h2' v2
  setTarget h3' v3
  nullF <- nullFace g
  setFace h0' nullF
  setFace h1' nullF
  setFace h2' nullF
  setFace h3' nullF
  opposite h3'

makeHexahedron :: MutableHalfedgeGraph m g v h e
               => MutableFaceGraph m g v h e f
               => PointGraph g v k
               => Property m g k p
               => Eq h
               => g -> p -> p -> p -> p -> p -> p -> p -> p -> m h
makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7 = do
  vs <- replicateM 8 (addVertex g)
  let [v0, v1, v2, v3, v4, v5, v6, v7] = vs
  ht <- formQuad g v4 v5 v6 v7
  hb <- prev =<< formQuad g v0 v3 v2 v1
  let
    worker (ht', hb') _ = do
      h <- halfedge =<< addEdge g
      setTarget h =<< target hb'
      setNext h =<< opposite hb'
      (setNext ?? h) =<< (opposite <=< prev) ht'
      h' <- opposite h
      setTarget h' =<< (source <=< prev) ht'
      setNext h' =<< (opposite <=< next <=< next) ht'
      (setNext ?? h) =<< (opposite <=< next) hb'
      liftM2 (,) (prev ht) (next hb)
    worker2 hb' _ = do
      Euler.fillHole g =<< opposite hb'
      next hb'
  (_, hb') <- foldM worker (ht, hb) [0..3]
  hb'' <- foldM worker2 hb' [0..3]

  mapM_ (uncurry $ replaceProperty g) (zip (point g <$> vs) [p0, p1, p2, p3, p4, p5, p6, p7])

  (next <=< next) hb''

makeRegularPrism :: MutableHalfedgeGraph m g v h e
                 => MutableFaceGraph m g v h e f
                 => PointGraph g v k
                 => Property m g k (p a)
                 => (Ord v, Eq f, Eq h)
                 => Floating a
                 => R3 p
                 => g -> Int -> p a -> a -> a -> Bool -> m h
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

  fromJust <$> halfedgeVV (vs ! 0) (vs ! 1)
