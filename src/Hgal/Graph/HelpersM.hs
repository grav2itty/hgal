module Hgal.Graph.HelpersM where

import Control.Monad
import Control.Lens ((??))

import Hgal.Graph.ClassM


setBorder :: MutableFaceGraph m g
          => g
          -> Halfedge g
          -> m ()
setBorder g h = setFace g h =<< nullFace g

copy :: MutableHalfedgeGraph m g
     => MutableFaceGraph m g
     => g
     -> Halfedge g
     -> m (Halfedge g)
copy g h = do
  e <- addEdge g
  res <- halfedgeE g e
  ropp <- opposite g res
  hopp <- opposite g h
  setTarget g res =<< target g h
  setTarget g hopp =<< target g hopp -- ??
  setFace g res =<< face g h
  setFace g ropp
    =<< face g hopp
  -- note that we cannot call set_next as it then would call set_prev on the  original
  return res

setVertexHalfedge :: MutableHalfedgeGraph m g
                  => g
                  -> Halfedge g
                  -> m ()
setVertexHalfedge g h = do
  (setHalfedgeV g ?? h) =<< target g h

closeTip :: MutableHalfedgeGraph m g
         => g
         -> Halfedge g
         -> Vertex g
         -> m ()
closeTip g h v = do
  setNext g h =<< opposite g h
  setTarget g h v
  setHalfedgeV g v h

insertTip :: MutableHalfedgeGraph m g
          => g
          -> Halfedge g
          -> Halfedge g
          -> m ()
insertTip g h h2 = do
  setNext g h =<< next g h2
  setNext g h2 =<< opposite g h
  setTarget g h =<< target g h2

removeTip :: MutableHalfedgeGraph m g
          => g
          -> Halfedge g
          -> m ()
removeTip g h = do
  setNext g h =<< (next g <=< opposite g <=< next g) h

setFaceInFaceLoop :: Eq (Halfedge g)
                  => MutableFaceGraph m g
                  => g
                  -> Halfedge g
                  -> Face g
                  -> m ()
setFaceInFaceLoop g h f = worker h
  where
    worker hx = do
      setFace g hx f
      n <- next g hx
      when (n /= h) (worker n)

insertHalfedge :: MutableHalfedgeGraph m g
               => MutableFaceGraph m g
               => g
               -> Halfedge g
               -> Halfedge g
               -> m ()
insertHalfedge g h f = do
  setNext g h =<< next g f
  setNext g f h
  setFace g h =<< face g f

exactNumVertices :: HalfedgeGraph m g
                 => g
                 -> m Int
exactNumVertices = return . length <=< vertices

exactNumHalfedges :: HalfedgeGraph m g
                  => g
                  -> m Int
exactNumHalfedges = return . length <=< halfedges

exactNumEdges :: HalfedgeGraph m g
              => g
              -> m Int
exactNumEdges = return . length <=< edges

exactNumFaces :: FaceGraph m g
              => g
              -> m Int
exactNumFaces = return . length <=< faces

isIsolated :: Eq (Halfedge g)
           => HalfedgeGraph m g
           => g
           -> Vertex g
           -> m Bool
isIsolated g v = liftM2 (==) (halfedgeV g v) (nullHalfedge g)

adjustIncomingHalfedge :: Eq (Vertex g)
                       => Eq (Halfedge g)
                       => Eq (Face g)
                       => MutableHalfedgeGraph m g
                       => FaceGraph m g
                       => g
                       -> Vertex g
                       -> m ()
adjustIncomingHalfedge g v = do
  h <- halfedgeV g v
  nullH <- nullHalfedge g
  nullF <- nullFace g
  if h == nullH then return ()
    else do
      tar <- target g h
      h' <- if tar == v then return h
              else opposite g h >>= \o -> setHalfedgeV g v o >> return o
      let worker hx = do
            f <- face g h
            if f == nullF then setHalfedgeV g v h
              else do
                n <- (opposite g <=< next g) hx
                when (hx /= h') (worker n)
      worker h'
