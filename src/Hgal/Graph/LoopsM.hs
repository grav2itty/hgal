module Hgal.Graph.LoopsM where

import Control.Monad
import Control.Monad.State
import Control.Lens ((??))
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV

import Hgal.Graph.ClassM


nextAroundTarget :: Monad m
                 => HalfedgeGraph m g
                 => g
                 -> Halfedge g
                 -> m (Halfedge g)
nextAroundTarget g = opposite g <=< next g

prevAroundTarget :: Monad m
                 => HalfedgeGraph m g
                 => g
                 -> Halfedge g
                 -> m (Halfedge g)
prevAroundTarget g = prev g <=< opposite g

nextAroundSource :: Monad m
                 => HalfedgeGraph m g
                 => g
                 -> Halfedge g
                 -> m (Halfedge g)
nextAroundSource g = next g <=< opposite g

prevAroundSource :: Monad m
                 => HalfedgeGraph m g
                 => g
                 -> Halfedge g
                 -> m (Halfedge g)
prevAroundSource g = opposite g <=< prev g

-------------------------------------------------------------------------------
-- perform action on each element

halfedgeAroundTarget :: Monad m
                     => Eq (Halfedge g)
                     => HalfedgeGraph m g
                     => g
                     -> (g -> Halfedge g -> m a)
                     -> Halfedge g
                     -> m a
halfedgeAroundTarget = loop nextAroundTarget

halfedgeAroundSource :: Monad m
                     => Eq (Halfedge g)
                     => HalfedgeGraph m g
                     => g
                     -> (g -> Halfedge g -> m a)
                     -> Halfedge g
                     -> m a
halfedgeAroundSource = loop nextAroundSource

halfedgeAroundFace :: Monad m
                   => Eq (Halfedge g)
                   => HalfedgeGraph m g
                   => g
                   -> (g -> Halfedge g -> m a)
                   -> Halfedge g
                   -> m a
halfedgeAroundFace = loop next

faceAroundTarget :: Monad m
                 => Eq (Halfedge g)
                 => FaceGraph m g
                 => g
                 -> (g -> Face g -> m a)
                 -> Halfedge g
                 -> m a
faceAroundTarget g f = halfedgeAroundTarget g (\g' hx -> f g' =<< face g' hx)

faceAroundFace :: Monad m
               => Eq (Halfedge g)
               => FaceGraph m g
               => g
               -> (g -> Face g -> m a)
               -> Halfedge g
               -> m a
faceAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< (face g' <=< opposite g') hx)

vertexAroundTarget :: Monad m
                   => Eq (Halfedge g)
                   => HalfedgeGraph m g
                   => g
                   -> (g -> Vertex g -> m a)
                   -> Halfedge g
                   -> m a
vertexAroundTarget g f =
  halfedgeAroundTarget g (\g' hx -> f g' =<< source g' hx)

vertexAroundFace :: Monad m
                 => Eq (Halfedge g)
                 => FaceGraph m g
                 => g
                 -> (g -> Vertex g -> m a)
                 -> Halfedge g
                 -> m a
vertexAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< target g' hx)

edgeAroundFace :: Monad m
               => Eq (Halfedge g)
               => HalfedgeGraph m g
               => g
               -> (g -> Edge g -> m a)
               -> Halfedge g
               -> m a
edgeAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< edge g' hx)

-------------------------------------------------------------------------------
-- gets circular vector of all elements

halfedgesAroundTarget :: Monad m
                      => Eq (Halfedge g)
                      => HalfedgeGraph m g
                      => g
                      -> Halfedge g
                      -> m (CircularVector (Halfedge g))
halfedgesAroundTarget g h =
  CV.unfoldr1M (loopC nextAroundTarget g (const return)) h h

halfedgesAroundSource :: Monad m
                      => Eq (Halfedge g)
                      => HalfedgeGraph m g
                      => g
                      -> Halfedge g
                      -> m (CircularVector (Halfedge g))
halfedgesAroundSource g h =
  CV.unfoldr1M (loopC nextAroundSource g (const return)) h h

halfedgesAroundFace :: Monad m
                    => Eq (Halfedge g)
                    => HalfedgeGraph m g
                    => g
                    -> Halfedge g
                    -> m (CircularVector (Halfedge g))
halfedgesAroundFace g h = CV.unfoldr1M (loopC next g (const return)) h h

facesAroundTarget :: Monad m
                  => Eq (Halfedge g)
                  => FaceGraph m g
                  => g
                  -> Halfedge g
                  -> m (CircularVector (Face g))
facesAroundTarget g h =
  (CV.unfoldr1M (loopC nextAroundTarget g face) ?? h) =<< face g h

facesAroundFace :: Monad m
                => Eq (Halfedge g)
                => FaceGraph m g
                => g
                -> Halfedge g
                -> m (CircularVector (Face g))
facesAroundFace g h =
  (CV.unfoldr1M (loopC next g face) ?? h) =<< face g h

verticesAroundTarget :: Monad m
                     => Eq (Halfedge g)
                     => HalfedgeGraph m g
                     => g
                     -> Halfedge g
                     -> m (CircularVector (Vertex g))
verticesAroundTarget g h =
  (CV.unfoldr1M (loopC nextAroundTarget g source) ?? h) =<< source g h

verticesAroundFace :: Monad m
                   => Eq (Halfedge g)
                   => HalfedgeGraph m g
                   => g
                   -> Halfedge g
                   -> m (CircularVector (Vertex g))
verticesAroundFace g h =
  (CV.unfoldr1M (loopC next g target) ?? h) =<< target g h

edgesAroundFace :: Monad m
                => Eq (Halfedge g)
                => HalfedgeGraph m g
                => g
                -> Halfedge g
                -> m (CircularVector (Edge g))
edgesAroundFace g h =
  (CV.unfoldr1M (loopC next g edge) ?? h) =<< edge g h

-------------------------------------------------------------------------------
-- internal helpers

loop :: Monad m
     => Eq (Halfedge g)
     => (g -> Halfedge g -> m (Halfedge g))
     -> g
     -> (g -> Halfedge g -> m a)
     -> Halfedge g
     -> m a
loop m g f h = worker h
  where
    worker hx = do
      n <- m g hx
      if n /= h then worker n else f g hx

loopC :: Monad m
      => Eq (Halfedge g)
      => (g -> Halfedge g -> m (Halfedge g))
      -> g
      -> (g -> Halfedge g -> m a)
      -> Halfedge g
      -> m (Maybe (a, Halfedge g))
loopC m g f h = worker h
  where
    worker hx = do
      n <- m g hx
      a <- f g hx
      if n /= h then return (Just (a, n)) else return Nothing
