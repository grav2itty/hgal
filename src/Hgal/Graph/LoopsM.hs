module Hgal.Graph.LoopsM where

import Control.Monad
import Control.Lens ((??))
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV

import Hgal.Graph.ClassM


nextAroundTarget :: HalfedgeGraph m g v h e
                 => h -> m h
nextAroundTarget = opposite <=< next

prevAroundTarget :: HalfedgeGraph m g v h e
                 => h -> m h
prevAroundTarget = prev <=< opposite

nextAroundSource :: HalfedgeGraph m g v h e
                 => h -> m h
nextAroundSource = next <=< opposite

prevAroundSource :: HalfedgeGraph m g v h e
                 => h -> m h
prevAroundSource = opposite <=< prev

-- -------------------------------------------------------------------------------
-- -- perform action on each element

halfedgeAroundTarget :: HalfedgeGraph m g v h e
                     => Eq h
                     => g -> (g -> h -> m ()) -> h -> m ()
halfedgeAroundTarget = loop nextAroundTarget

halfedgeAroundSource :: HalfedgeGraph m g v h e
                     => Eq h
                     => g -> (g -> h -> m ()) -> h -> m ()
halfedgeAroundSource = loop nextAroundSource

halfedgeAroundFace :: HalfedgeGraph m g v h e
                   => Eq h
                   => g -> (g -> h -> m ()) -> h -> m ()
halfedgeAroundFace = loop next

faceAroundTarget :: FaceGraph m g v h e f
                 => Eq h
                 => g -> (g -> f -> m ()) -> h -> m ()
faceAroundTarget g f = halfedgeAroundTarget g (\g' hx -> f g' =<< face hx)

faceAroundFace :: FaceGraph m g v h e f
               => Eq h
               => g -> (g -> f -> m ()) -> h -> m ()
faceAroundFace g f = halfedgeAroundFace g (\g' hx -> f g' =<< (face <=< opposite) hx)

vertexAroundTarget :: HalfedgeGraph m g v h e
                   => Eq h
                   => g -> (g -> v -> m ()) -> h -> m ()
vertexAroundTarget g f =
  halfedgeAroundTarget g (\g' hx -> f g' =<< source hx)

vertexAroundFace :: FaceGraph m g v h e f
                 => Eq h
                 => g -> (g -> v -> m ()) -> h -> m ()
vertexAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< target hx)

edgeAroundFace :: HalfedgeGraph m g v h e
               => Eq h
               => g -> (g -> e -> m ()) -> h -> m ()
edgeAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< edge hx)

-- -------------------------------------------------------------------------------
-- -- gets circular vector of all elements

halfedgesAroundTarget :: HalfedgeGraph m g v h e
                      => Eq h
                      => h -> m (CircularVector h)
halfedgesAroundTarget h =
  CV.unfoldr1M (loopC nextAroundTarget return h) h h

halfedgesAroundSource :: HalfedgeGraph m g v h e
                      => Eq h
                      => h -> m (CircularVector h)
halfedgesAroundSource h =
  CV.unfoldr1M (loopC nextAroundSource return h) h h

halfedgesAroundFace :: HalfedgeGraph m g v h e
                    => Eq h
                    => h -> m (CircularVector h)
halfedgesAroundFace h = CV.unfoldr1M (loopC next return h) h h

facesAroundTarget :: FaceGraph m g v h e f
                  => Eq h
                  => h -> m (CircularVector f)
facesAroundTarget h =
  (CV.unfoldr1M (loopC nextAroundTarget face h) ?? h) =<< face h

facesAroundFace :: FaceGraph m g v h e f
                => Eq h
                => h -> m (CircularVector f)
facesAroundFace h =
  (CV.unfoldr1M (loopC next face h) ?? h) =<< face h

verticesAroundTarget :: HalfedgeGraph m g v h e
                     => Eq h
                     => h -> m (CircularVector v)
verticesAroundTarget h =
  (CV.unfoldr1M (loopC nextAroundTarget source h) ?? h) =<< source h

verticesAroundFace :: HalfedgeGraph m g v h e
                   => Eq h
                   => h -> m (CircularVector v)
verticesAroundFace h =
  (CV.unfoldr1M (loopC next target h) ?? h) =<< target h

edgesAroundFace :: HalfedgeGraph m g v h e
                => Eq h
                => h -> m (CircularVector e)
edgesAroundFace h =
  (CV.unfoldr1M (loopC next edge h) ?? h) =<< edge h

-- -------------------------------------------------------------------------------
-- -- internal helpers

loop :: Monad m
     => Eq h
     => (h -> m h)
     -> g
     -> (g -> h -> m ())
     -> h
     -> m ()
loop m g f h = worker h
  where
    worker hx = do
      n <- m hx
      if n /= h then worker n else f g hx

loopC :: Monad m
      => Eq h
      => (h -> m h)
      -> (h -> m a)
      -> h
      -> h
      -> m (Maybe (a, h))
loopC m f h hx = do
  n <- m hx
  a <- f n
  if n /= h then return (Just (a, n)) else return Nothing
