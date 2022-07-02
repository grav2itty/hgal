module Hgal.Graph.ClassM where

import Control.Lens
import Control.Monad.State
import Data.Kind

import qualified Hgal.Graph.Class as Pure


type family VertexDescriptor a :: Type
type family EdgeDescriptor a :: Type
type family HalfedgeDescriptor a :: Type

type Vertex a = VertexDescriptor a
type Edge a = EdgeDescriptor a
type Halfedge a = HalfedgeDescriptor a

class Monad m => HalfedgeGraph m g | g -> m where
  edge :: g -> Halfedge g -> m (Edge g)
  halfedgeE :: g -> Edge g -> m (Halfedge g)
  halfedgeV :: g -> Vertex g -> m (Halfedge g)
  halfedgeVV :: g -> Vertex g -> Vertex g -> m (Maybe (Halfedge g))
  opposite :: g -> Halfedge g -> m (Halfedge g)
  source :: g -> Halfedge g -> m (Vertex g)
  target :: g -> Halfedge g -> m (Vertex g)
  next :: g -> Halfedge g -> m (Halfedge g)
  prev :: g -> Halfedge g -> m (Halfedge g)

  isBorderH :: g -> Halfedge g -> m Bool
  isBorderV :: g -> Vertex g -> m Bool
  nullHalfedge :: g -> m (Halfedge g)

  showM :: g -> m String

--   default edge :: HalfedgeGraphS m g => g -> Halfedge g -> m (Edge g)
--   edge _ h = gets (`Pure.edge` h)

class (Monad m, HalfedgeGraph m g) => MutableHalfedgeGraph m g | g -> m where
  addVertex :: g -> m (Vertex g)
  removeVertex :: g -> Vertex g -> m ()
  addEdge :: g -> m (Edge g)
  removeEdge :: g -> Edge g -> m ()
  setTarget :: g -> Halfedge g -> Vertex g -> m ()
  setNext :: g -> Halfedge g -> Halfedge g -> m ()
  setHalfedgeV :: g -> Vertex g -> Halfedge g -> m ()


type family FaceDescriptor a :: Type

type Face a = FaceDescriptor a

class (Monad m, HalfedgeGraph m g) => FaceGraph m g | g -> m where
  face :: g -> Halfedge g -> m (Face g)
  halfedgeF :: g -> Face g -> m (Halfedge g)

  nullFace :: g -> m (Face g)

class (Monad m, FaceGraph m g) => MutableFaceGraph m g | g -> m where
  addFace :: g -> m (Face g)
  removeFace :: g -> Face g -> m ()
  setFace :: g -> Halfedge g -> Face g -> m ()
  setHalfedgeF :: g -> Face g -> Halfedge g -> m ()

-- class
--   ( Monad m, MonadState g m,
--     Pure.HalfedgeGraph g,
--     Pure.Vertex g ~ Vertex g,
--     Pure.Halfedge g ~ Halfedge g,
--     Pure.Edge g ~ Edge g
--   ) => HalfedgeGraphS m g where

-- class
--   ( HalfedgeGraphS m g,
--     Pure.MutableHalfedgeGraph g
--   ) => MutableHalfedgeGraphS m g where

-- class
--   ( HalfedgeGraphS m g,
--     Pure.FaceGraph g,
--     Pure.Face g ~ Face g
--   ) => FaceGraphS m g where

-- class
--   ( FaceGraphS m g,
--     Pure.MutableFaceGraph g
--   ) => MutableFaceGraphS m g where

type family PointDescriptor a :: Type
type Point a = PointDescriptor a

class PointGraph g where
  point :: g -> Vertex g -> Point g

