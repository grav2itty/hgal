module Hgal.Graph.Class where

class Element g a where
  isBorder :: g -> a -> Bool
  isValid :: g -> a -> Bool

class Element g a => RemovableElement g a where
  remove :: g -> a -> g

class GetHalfedge g a h | a -> h where
  halfedge :: g -> a -> h

class SetHalfedge g a h | a -> h where
  setHalfedge :: g -> a -> h -> g

class GetFace g a f | a -> f where
  face :: g -> a -> f

class SetFace g a f | a -> f where
  setFace :: g -> a -> f -> g

class HalfedgeC g v h e | h -> v, h -> e where
  edge :: g -> h -> e
  opposite :: g -> h -> h
  source :: g -> h -> v
  target :: g -> h -> v
  next :: g -> h -> h
  prev :: g -> h -> h

  halfedgeVV :: g -> v -> v -> Maybe h

class MutableHalfedgeC g v h | h -> v where
  setTarget :: g -> h -> v -> g
  setNext :: g -> h -> h -> g


class
  ( Element g v,
    Element g h,
    Element g e,
    GetHalfedge g v h,
    GetHalfedge g e h,
    HalfedgeC g v h e
  ) => HalfedgeGraph g v h e | g -> h, g -> v, g -> e where

  vertices :: g -> [v]
  halfedges :: g -> [h]
  edges :: g -> [e]

  nullVertex :: g -> v
  nullHalfedge :: g -> h
  nullEdge :: g -> e

class
  ( HalfedgeGraph g v h e,
    RemovableElement g v,
    RemovableElement g e,
    SetHalfedge g v h,
    MutableHalfedgeC g v h
  ) => MutableHalfedgeGraph g v h e | g -> h, g -> v, g -> e where

  addVertex :: g -> (v, g)
  addEdge :: g -> (e, g)

class
  ( HalfedgeGraph g v h e,
    Element g f,
    GetHalfedge g f h,
    GetFace g h f
  ) => FaceGraph g v h e f | g -> v, g -> h, g -> e, g -> f where

  faces :: g -> [f]

  nullFace :: g -> f

class
  ( FaceGraph g v h e f,
    RemovableElement g f,
    SetHalfedge g f h,
    SetFace g h f
  ) => MutableFaceGraph g v h e f | g -> v, g -> h, g -> e, g -> f where

  addFace :: g -> (f, g)
