module Hgal.Graph.GeneratorsM where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import Linear

import Hgal.Graph.Class (Point(..))
import Hgal.Graph.ClassM
import Hgal.Data.PropertyM
import qualified Hgal.Graph.EulerOperationsM as Euler


makeTriangle :: MutableFaceGraph m g v h e f
             => PointGraph m g v p
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

  replaceProperty g (Point v0) p0
  replaceProperty g (Point v1) p1
  replaceProperty g (Point v2) p2

  opposite h2'

makeQuad :: MutableFaceGraph m g v h e f
         => PointGraph m g v p
         => g -> p -> p -> p -> p -> m h
makeQuad g p0 p1 p2 p3 = do
  v0 <- addVertex g
  v1 <- addVertex g
  v2 <- addVertex g
  v3 <- addVertex g
  replaceProperty g (Point v0) p0
  replaceProperty g (Point v1) p1
  replaceProperty g (Point v2) p2
  replaceProperty g (Point v3) p3
  formQuad g v0 v1 v2 v3

formQuad :: MutableFaceGraph m g v h e f
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

makeTetrahedron :: MutableFaceGraph m g v h e f
                => PointGraph m g v p
                => g -> p -> p -> p -> p -> m h
makeTetrahedron g p0 p1 p2 p3 = do
  v0 <- addVertex g
  v2 <- addVertex g -- this and the next line are switched to keep points in order
  v1 <- addVertex g
  v3 <- addVertex g
  h0 <- halfedge =<< addEdge g
  h1 <- halfedge =<< addEdge g
  h2 <- halfedge =<< addEdge g
  setNext h0 h1
  setNext h1 h2
  setNext h2 h0
  setTarget h0 v1
  setTarget h1 v2
  setTarget h2 v0
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
  h3 <- halfedge =<< addEdge g
  h4 <- halfedge =<< addEdge g
  h5 <- halfedge =<< addEdge g
  setTarget h3 v3
  setTarget h4 v3
  setTarget h5 v3
  setHalfedge v3 h3
  setNext h0' h3
  setNext h1' h4
  setNext h2' h5
  setNext h3 =<< opposite h4
  setNext h4 =<< opposite h5
  setNext h5 =<< opposite h3
  (setNext ?? h0') =<< opposite h4
  (setNext ?? h1') =<< opposite h5
  (setNext ?? h2') =<< opposite h3
  (setTarget ?? v0) =<< opposite h3
  (setTarget ?? v1) =<< opposite h4
  (setTarget ?? v2) =<< opposite h5

  f2 <- addFace g
  setHalfedge f2 h0'
  setFace h0' f2
  setFace h3 f2
  (setFace ?? f2) =<< opposite h4
  f3 <- addFace g
  setHalfedge f3 h1'
  setFace h1' f3
  setFace h4 f3
  (setFace ?? f3) =<< opposite h5
  f4 <- addFace g
  setHalfedge f4 h2'
  setFace h2' f4
  setFace h5 f4
  (setFace ?? f4) =<< opposite h3

  replaceProperty g (Point v0) p0
  replaceProperty g (Point v1) p2 -- this and the next line are switched to reorient the surface
  replaceProperty g (Point v2) p1
  replaceProperty g (Point v3) p2

  opposite h2'

makeHexahedron :: MutableFaceGraph m g v h e f
               => PointGraph m g v p
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
      (setNext ?? h') =<< (opposite <=< next) hb'
      liftM2 (,) (prev ht') (next hb')
    worker2 hb' _ = do
      Euler.fillHole g =<< opposite hb'
      next hb'
  (_, hb') <- foldM worker (ht, hb) [0..3]
  hb'' <- foldM worker2 hb' [0..3]

  mapM_ (uncurry $ replaceProperty g) (zip (Point <$> vs) [p0, p1, p2, p3, p4, p5, p6, p7])

  (next <=< next) hb''

makeRegularPrism :: MutableFaceGraph m g v h e f
                 => PointGraph m g v (p a)
                 => (Ord v, Eq h, Eq f)
                 => Floating a
                 => R3 p
                 => g -> Int -> p a -> a -> a -> Bool -> m h
makeRegularPrism g n center height radius isClosed = do
  let step = assert (n >= 3) $
             2 * pi / fromIntegral n

  vs <- V.replicateM (2*n) (addVertex g)

  forM_ [0..n-1] $ \i -> do
    let i' = fromIntegral i
        p1 = center & _x +~ (radius * cos (i' * step))
                    & _z -~ (radius * sin (i' * step))
        p2 = p1 & _y +~ height
    replaceProperty g (Point $ vs ! (i + n)) p1
    replaceProperty g (Point $ vs ! i) p2

  forM_ [0..n-1] $ \i -> do
    let ii = mod (i+1) n
    Euler.addFace g [vs ! ii, vs ! i, vs ! (ii + n)]
    Euler.addFace g [vs ! (ii + n), vs ! i, vs ! (i + n)]

  when isClosed $ do
    top <- addVertex g
    bot <- addVertex g
    replaceProperty g (Point top) (_y +~ height $ center)
    replaceProperty g (Point bot) center

    forM_ [0..n-1] $ \i -> do
      let ii = mod (i+1) n
      Euler.addFace g [vs ! i, vs ! ii, top]
      Euler.addFace g [bot, vs ! (ii + n), vs ! (i + n)]

  fromJust <$> halfedgeVV (vs ! 0) (vs ! 1)

makePyramid :: MutableFaceGraph m g v h e f
            => PointGraph m g v (p a)
            => (Ord v, Eq h, Eq f)
            => Floating a
            => R3 p
            => g -> Int -> p a -> a -> a -> Bool -> m h
makePyramid g n center height radius isClosed = do
  let step = assert (n >= 3) $
             2 * pi / fromIntegral n

  apex <- addVertex g
  vs <- V.replicateM n (addVertex g)

  replaceProperty g (Point apex) (center & _y +~ height)

  forM_ [0..n-1] $ \i -> do
    let i' = fromIntegral i
        p = center & _x +~ (radius * cos (i' * step))
                   & _z -~ (radius * sin (i' * step))
    replaceProperty g (Point $ vs ! i) p

  forM_ [0..n-1] $ \i -> do
    let ii = mod (i+1) n
    Euler.addFace g [apex, vs ! i, vs ! ii]

  when isClosed $ do
    bot <- addVertex g
    replaceProperty g (Point bot) center

    forM_ [0..n-1] $ \i -> do
      let ii = mod (i+1) n
      Euler.addFace g [bot, vs ! ii, vs ! i]

  fromJust <$> halfedgeVV (vs ! 0) apex

makeIcosahedron :: MutableFaceGraph m g v h e f
                => PointGraph m g v (p a)
                => (Ord v, Eq h, Eq f)
                => Floating a
                => R3 p
                => g -> p a -> a -> m h
makeIcosahedron g center radius = do

  vs <- V.replicateM 12 (addVertex g)

  let phi = (1 + sqrt 5) * 0.5
      t = radius / sqrt (1 + phi*phi)
      tphi = t * phi

      ps = V.fromList $ ($ center) <$>
        [ _yz +~ V2 t tphi
        , _yz +~ V2 t (-tphi)
        , _yz +~ V2 (-t) tphi
        , _yz +~ V2 (-t) (-tphi)
        , _xy +~ V2 t tphi
        , _xy +~ V2 t (-tphi)
        , _xy +~ V2 (-t) tphi
        , _xy +~ V2 (-t) (-tphi)
        , _xz +~ V2 tphi t
        , _xz +~ V2 tphi (-t)
        , _xz +~ V2 (-tphi) t
        , _xz +~ V2 (-tphi) (-t)
        ]

  V.zipWithM_ (replaceProperty g . Point) vs ps

  let faceI =
        [ (0, 2, 8)
        , (0, 8, 4)
        , (0, 4, 6)
        , (0, 6, 10)
        , (0, 10, 2)
        , (1, 9, 3)
        , (1, 3, 11)
        , (1, 11, 6)
        , (1, 6, 4)
        , (1, 4, 9)
        , (5, 8, 2)
        , (5, 2, 7)
        , (5, 7, 3)
        , (5, 3, 9)
        , (5, 9, 8)
        , (8, 9, 4)
        , (3, 7, 11)
        , (11, 7, 10)
        , (10, 7, 2)
        , (6, 11, 10)
        ]

  mapM_ (\(i, j, k) -> Euler.addFace g [vs ! i, vs ! j, vs ! k] ) faceI

  fromJust <$> halfedgeVV (vs ! 5) (vs ! 0)

makeGrid :: MutableFaceGraph m g v h e f
         => PointGraph m g v p
         => (Ord v, Eq h, Eq f)
         => g -> Int -> Int -> (Int -> Int -> p) -> Bool -> m h
makeGrid g i j coordF triangulated = do
  vs <- V.fromList <$> sequence
    [ addVertex g >>= \v -> replaceProperty g (Point v) (coordF x y) >> return v
    | x <- [0..i], y <- [0..j]
    ]
  sequence
    [ if triangulated then
        Euler.addFace g [v0, v1, v3] >>
        Euler.addFace g [v1, v2, v3]
      else
        Euler.addFace g fvs
    | x <- [0..(i - 1)], y <- [0..(j - 1)]
    , let i0 = i2 - (i + 1) - 1
          i1 = i0 + 1
          i2 = (i + 1) * (y + 1) + x + 1
          i3 = i2 - 1
          fvs@[v0, v1, v2, v3] = (vs !) <$> [i0, i1, i2, i3]
    ]

  fromJust <$> halfedgeVV (vs ! 1) (vs ! 0)
