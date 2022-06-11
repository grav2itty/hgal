module Hgal.Graph.ClassM where

import Data.Kind


type family VertexDescriptor a :: Type
type family EdgeDescriptor a :: Type
type family HalfedgeDescriptor a :: Type

type Vertex a = VertexDescriptor a
type Edge a = EdgeDescriptor a
type Halfedge a = HalfedgeDescriptor a

class Monad m => HalfedgeGraph m g where
  edge :: g -> Halfedge g -> m (Edge g)
  halfedgeE :: g -> Edge g -> m (Halfedge g)
  halfedgeV :: g -> Vertex g -> m (Halfedge g)
  opposite :: g -> Halfedge g -> m (Halfedge g)
  source :: g -> Halfedge g -> m (Vertex g)
  target :: g -> Halfedge g -> m (Vertex g)
  next :: g -> Halfedge g -> m (Halfedge g)
  prev :: g -> Halfedge g -> m (Halfedge g)

  isBorder :: g -> Halfedge g -> m Bool

  showM :: g -> m String

class (Monad m, HalfedgeGraph m g) => MutableHalfedgeGraph m g where
  addVertex :: g -> m (Vertex g)
  removeVertex :: g -> Vertex g -> m ()
  addEdge :: g -> m (Edge g)
  removeEdge :: g -> Edge g -> m ()
  setTarget :: g -> Halfedge g -> Vertex g -> m ()
  setNext :: g -> Halfedge g -> Halfedge g -> m ()
  setHalfedgeV :: g -> Vertex g -> Halfedge g -> m ()


type family FaceDescriptor a :: Type

type Face a = FaceDescriptor a

class (Monad m, HalfedgeGraph m g) => FaceGraph m g where
  face :: g -> Halfedge g -> m (Face g)
  halfEdgeF :: g -> Face g -> m (Halfedge g)
  nullFace :: g -> m (Face g)

class (Monad m, FaceGraph m g) => MutableFaceGraph m g where
  addFace :: g -> m (Face g)
  removeFace :: g -> Face g -> m ()
  setFace :: g -> Halfedge g -> Face g -> m ()
  setHalfedgeF :: g -> Face g -> Halfedge g -> m ()
