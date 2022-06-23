module Hgal.Graph.Helpers where

import Control.Monad
import Control.Lens ((??))
import Data.Either
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV

import Hgal.Graph.ClassM


-- infixl 1 ??
-- (??) :: Functor f => f (a -> b) -> a -> f b
-- fab ?? a = fmap ($ a) fab

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
                  => Eq (Halfedge g)
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

insertHalfedge :: Monad m
               => MutableHalfedgeGraph m g
               => MutableFaceGraph m g
               => g
               -> Halfedge g
               -> Halfedge g
               -> m ()
insertHalfedge g h f = do
  setNext g h =<< next g f
  setNext g f h
  setFace g h =<< face g f

isIsolated :: Monad m
           => Eq (Halfedge g)
           => HalfedgeGraph m g
           => g
           -> Vertex g
           -> m Bool
isIsolated g v = liftM2 (==) (halfedgeV g v) (nullHalfedge  g)

adjustIncomingHalfedge :: Monad m
                       => Eq (Vertex g)
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


halfedgesAroundTarget :: Monad m
                      => Eq (Halfedge g)
                      => HalfedgeGraph m g
                      => g
                      -> (g -> Halfedge g -> m())
                      -> Halfedge g
                      -> m ()
halfedgesAroundTarget g f h = worker h
  where
    worker hx = do
      f g hx
      n <- (opposite g <=< next g) hx
      when (n /= h) (worker n)

halfedgesAroundTarget0 :: Monad m
                       => Eq (Halfedge g)
                       => HalfedgeGraph m g
                       => g
                       -> Halfedge g
                       -> m (CircularVector (Halfedge g))
halfedgesAroundTarget0 g h = CV.unfoldr1M worker h h
  where
    worker hx = do
      n <- (opposite g <=< next g) hx
      if n /= h then return (Just (n, n)) else return Nothing
