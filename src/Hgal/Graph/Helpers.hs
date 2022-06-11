module Hgal.Graph.Helpers where

import Control.Monad

import Hgal.Graph.ClassM


infixl 1 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

setVertexHalfedge :: Monad m
                  => MutableHalfedgeGraph m g
                  => g
                  -> Halfedge g
                  -> m ()
setVertexHalfedge g h = do
  (setHalfedgeV g ?? h) =<< target g h

closeTip :: Monad m
         => MutableHalfedgeGraph m g
         => g
         -> Halfedge g
         -> Vertex g
         -> m ()
closeTip g h v = do
  setNext g h =<< opposite g h
  setTarget g h v
  setHalfedgeV g v h

insertTip :: Monad m
          => MutableHalfedgeGraph m g
          => g
          -> Halfedge g
          -> Halfedge g
          -> m ()
insertTip g h h2 = do
  setNext g h =<< next g h2
  setNext g h2 =<< opposite g h
  setTarget g h =<< target g h2

removeTip :: Monad m
          => MutableHalfedgeGraph m g
          => g
          -> Halfedge g
          -> m ()
removeTip g h = do
  setNext g h =<< (next g <=< opposite g <=< next g) h

setFaceInFaceLoop :: Monad m
                  => Eq (HalfedgeDescriptor g)
                  => MutableFaceGraph m g
                  => g
                  -> Halfedge g
                  -> FaceDescriptor g
                  -> m ()
setFaceInFaceLoop g h f = worker h
  where
    worker hx = do
      setFace g hx f
      n <- next g hx
      when (n /= h) (worker n)
