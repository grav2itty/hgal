module Hgal.Graph.Class where

import Control.Lens (Lens')
import Data.Kind


type family VertexDescriptor a :: Type
type family EdgeDescriptor a :: Type
type family HalfedgeDescriptor a :: Type

type Vertex a = VertexDescriptor a
type Edge a = EdgeDescriptor a
type Halfedge a = HalfedgeDescriptor a

class HalfedgeGraph g where
  edge :: g -> Halfedge g -> Edge g
  halfedgeE :: g -> Edge g -> Halfedge g
  halfedgeV :: g -> Vertex g -> Halfedge g
  halfedgeVV :: g -> Vertex g -> Vertex g -> Maybe (Halfedge g)
  opposite :: g -> Halfedge g -> Halfedge g
  source :: g -> Halfedge g -> Vertex g
  target :: g -> Halfedge g -> Vertex g
  next :: g -> Halfedge g -> Halfedge g
  prev :: g -> Halfedge g -> Halfedge g

  isBorderH :: g -> Halfedge g -> Bool
  isBorderV :: g -> Vertex g -> Bool
  nullHalfedge :: g -> Halfedge g


class HalfedgeGraph g => MutableHalfedgeGraph g where
  addVertex :: g -> (Vertex g, g)
  removeVertex :: g -> Vertex g -> g
  addEdge :: g -> (Edge g, g)
  removeEdge :: g -> Edge g -> g
  setTarget :: g -> Halfedge g -> Vertex g -> g
  setNext :: g -> Halfedge g -> Halfedge g -> g
  setHalfedgeV :: g -> Vertex g -> Halfedge g -> g


type family FaceDescriptor a :: Type

type Face a = FaceDescriptor a

class HalfedgeGraph g => FaceGraph g where
  face :: g -> Halfedge g -> Face g
  halfedgeF :: g -> Face g -> Halfedge g

  nullFace :: g -> Face g

class FaceGraph g => MutableFaceGraph g where
  addFace :: g -> (Face g, g)
  removeFace :: g -> Face g -> g
  setFace :: g -> Halfedge g -> Face g -> g
  setHalfedgeF :: g -> Face g -> Halfedge g -> g
