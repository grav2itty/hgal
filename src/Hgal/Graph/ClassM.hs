module Hgal.Graph.ClassM where

import Control.Lens(Iso'())
import Control.Monad.State

import qualified Hgal.Graph.Class as Pure
import Hgal.Data.PropertyM


class Element m a where
  isBorder :: a -> m Bool
  isValid :: a -> m (Either String Bool)
  degree :: a -> m Int

  default isBorder :: Pure.Element g a
                   => MonadState g m
                   => a -> m Bool
  isBorder a = gets (`Pure.isBorder` a)

  default isValid :: Pure.Element g a
                  => MonadState g m
                  => a -> m (Either String Bool)
  isValid a = gets (`Pure.isValid` a)

  default degree :: Pure.Element g a
                 => MonadState g m
                 => a -> m Int
  degree a = gets (`Pure.degree` a)


class Element m a => RemovableElement m a where
  remove :: a -> m ()

  default remove :: Pure.RemovableElement g a
                 => MonadState g m
                 => a -> m ()
  remove a = modify (`Pure.remove` a)


class GetHalfedge m a h | a -> h where
  halfedge :: a -> m h

  default halfedge :: Pure.GetHalfedge g a h
                   => MonadState g m
                   => a -> m h
  halfedge a = gets (`Pure.halfedge` a)


class SetHalfedge m a h | a -> h where
  setHalfedge :: a -> h -> m ()

  default setHalfedge :: Pure.SetHalfedge g a h
                      => MonadState g m
                      => a -> h -> m ()
  setHalfedge a h = modify (\s -> Pure.setHalfedge s a h)


class GetFace m a f | a -> f where
  face :: a -> m f

  default face :: Pure.GetFace g a f
               => MonadState g m
               => a -> m f
  face a = gets (`Pure.face` a)


class SetFace m a f | a -> f where
  setFace :: a -> f -> m ()

  default setFace :: Pure.SetFace g a f
                  => MonadState g m
                  => a -> f -> m ()
  setFace a f = modify (\s -> Pure.setFace s a f)


class HalfedgeC m v h e | h -> v, h -> e where
  edge :: h -> m e
  opposite :: h -> m h
  source :: h -> m v
  target :: h -> m v
  next :: h -> m h
  prev :: h -> m h

  halfedgeVV :: v -> v -> m (Maybe h)

  default edge :: Pure.HalfedgeC g v h e
               => MonadState g m
               => h -> m e
  edge h = gets (`Pure.edge` h)

  default opposite :: Pure.HalfedgeC g v h e
                   => MonadState g m
                   => h -> m h
  opposite h = gets (`Pure.opposite` h)

  default source :: Pure.HalfedgeC g v h e
                 => MonadState g m
                 => h -> m v
  source h = gets (`Pure.source` h)

  default target :: Pure.HalfedgeC g v h e
                 => MonadState g m
                 => h -> m v
  target h = gets (`Pure.target` h)

  default next :: Pure.HalfedgeC g v h e
               => MonadState g m
               => h -> m h
  next h = gets (`Pure.next` h)

  default prev :: Pure.HalfedgeC g v h e
               => MonadState g m
               => h -> m h
  prev h = gets (`Pure.prev` h)

  default halfedgeVV :: Pure.HalfedgeC g v h e
                     => MonadState g m
                     => v -> v -> m (Maybe h)
  halfedgeVV v1 v2 = gets (\s -> Pure.halfedgeVV s v1 v2)


class MutableHalfedgeC m v h | h -> v where
  setTarget :: h -> v -> m ()
  setNext :: h -> h -> m ()

  default setTarget :: Pure.MutableHalfedgeC g v h
                    => MonadState g m
                    => h -> v -> m ()
  setTarget h v = modify (\s -> Pure.setTarget s h v)

  default setNext :: Pure.MutableHalfedgeC g v h
                  => MonadState g m
                  => h -> h -> m ()
  setNext h1 h2 = modify (\s -> Pure.setNext s h1 h2)


-- either Monad or Elements (v, h, e, f) must hold or reference Graph
-- but since Monad is not required to do so (ST s)
-- functions that do not take any Element must take Graph

class
  ( Monad m,
    Element m v,
    Element m h,
    Element m e,
    GetHalfedge m v h,
    GetHalfedge m e h,
    HalfedgeC m v h e
  ) => HalfedgeGraph m g v h e | m -> g, g -> h, g -> v, g -> e where

  vertices :: g -> m [v]
  halfedges :: g -> m [h]
  edges :: g -> m [e]

  nullVertex :: g -> m v
  nullHalfedge :: g -> m h
  nullEdge :: g -> m e

  default vertices :: Pure.HalfedgeGraph g v h e
                   => MonadState g m
                   => g -> m [v]
  vertices _ = gets Pure.vertices

  default halfedges :: Pure.HalfedgeGraph g v h e
                    => MonadState g m
                    => g -> m [h]
  halfedges _ = gets Pure.halfedges

  default edges :: Pure.HalfedgeGraph g v h e
                => MonadState g m
                => g -> m [e]
  edges _ = gets Pure.edges

  default nullVertex :: Pure.HalfedgeGraph g v h e
                     => MonadState g m
                     => g -> m v
  nullVertex _ = gets Pure.nullVertex

  default nullHalfedge :: Pure.HalfedgeGraph g v h e
                       => MonadState g m
                       => g -> m h
  nullHalfedge _ = gets Pure.nullHalfedge

  default nullEdge :: Pure.HalfedgeGraph g v h e
                   => MonadState g m
                   => g -> m e
  nullEdge _ = gets Pure.nullEdge


class
  ( HalfedgeGraph m g v h e,
    RemovableElement m v,
    RemovableElement m e,
    SetHalfedge m v h,
    MutableHalfedgeC m v h
  ) => MutableHalfedgeGraph m g v h e | m -> g, g -> h, g -> v, g -> e where

  addVertex :: g -> m v
  addEdge :: g -> m e

  default addVertex :: Pure.MutableHalfedgeGraph g v h e
                    => MonadState g m
                    => g -> m v
  addVertex _ = state Pure.addVertex

  default addEdge :: Pure.MutableHalfedgeGraph g v h e
                  => MonadState g m
                  => g -> m e
  addEdge _ = state Pure.addEdge


class
  ( HalfedgeGraph m g v h e,
    Element m f,
    GetHalfedge m f h,
    GetFace m h f
  ) => FaceGraph m g v h e f | m -> g, g -> v, g -> h, g -> e, g -> f where

  faces :: g -> m [f]

  nullFace :: g -> m f

  default faces :: Pure.FaceGraph g v h e f
                => MonadState g m
                => g -> m [f]
  faces _ = gets Pure.faces

  default nullFace :: Pure.FaceGraph g v h e f
                   => MonadState g m
                   => g -> m f
  nullFace _ = gets Pure.nullFace


class
  ( FaceGraph m g v h e f,
    MutableHalfedgeGraph m g v h e,
    RemovableElement m f,
    SetHalfedge m f h,
    SetFace m h f
  ) => MutableFaceGraph m g v h e f | m -> g, g -> v, g -> h, g -> e, g -> f where

  addFace :: g -> m f

  default addFace :: Pure.MutableFaceGraph g v h e f
                  => MonadState g m
                  => g -> m f
  addFace _ = state Pure.addFace


class Property m g (Pure.Point v) p => PointGraph m g v p where

