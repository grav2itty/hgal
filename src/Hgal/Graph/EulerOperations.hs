module Hgal.Graph.EulerOperations where

import Control.Monad

import Hgal.Graph.Helpers
import Hgal.Graph.ClassM

import Debug.Trace


addCenterVertex :: Monad m
                => Eq (Halfedge g)
                => MutableHalfedgeGraph m g
                => MutableFaceGraph m g
                => g
                -> Halfedge g
                -> m (Halfedge g)
addCenterVertex g h = do
  hnew <- halfedgeE g =<< addEdge g
  vnew <- addVertex g
  closeTip g hnew vnew

  (insertTip g ?? h) =<< opposite g hnew
  setFace g hnew =<< face g h
  (setHalfedgeF g ?? h) =<< face g h

  h2 <- next g =<< opposite g hnew
  let
    worker hx = do
      n <- next g hx
      when (n /= hnew) $ do
        gnew <- halfedgeE g =<< addEdge g
        insertTip g gnew hnew
        (insertTip g ?? hx) =<< opposite g gnew
        fnew <- addFace g
        setFace g hx fnew
        setFace g gnew fnew
        (setFace g ?? fnew) =<< next g gnew
        (setHalfedgeF g ?? hx) =<< face g hx
        (worker <=< next g <=< opposite g) gnew
  worker h2

  nhnew <- next g hnew
  setFace g nhnew =<< face g hnew
  setVertexHalfedge g hnew
  return hnew

removeCenterVertex :: Monad m
                => Eq (Halfedge g)
                => MutableHalfedgeGraph m g
                => MutableFaceGraph m g
                => g
                -> Halfedge g
                -> m (Halfedge g)
removeCenterVertex g h = do
  h2 <- (opposite g <=< next g) h
  hret <- prev g h
  let
    worker hx = when (hx /= h) $ do
      gprev <- prev g hx
      setVertexHalfedge g gprev
      removeTip g gprev

      removeFace g =<< face g hx

      gnext <- (opposite g <=< next g) hx
      removeEdge g =<< edge g hx
      worker gnext
  worker h2
  setVertexHalfedge g hret
  removeTip g hret
  removeVertex g =<< target g h
  removeEdge g =<< edge g h
  setFaceInFaceLoop g hret =<< face g hret
  (setHalfedgeF g ?? hret) =<< face g hret

  return hret

joinFace :: Monad m
         => Eq (Halfedge g)
         => MutableHalfedgeGraph m g
         => MutableFaceGraph m g
         => g
         -> Halfedge g
         -> m (Halfedge g)
joinFace g h = do
  hop <- opposite g h
  hprev <- prev g h
  gprev <- prev g hop
  f <- face g h
  f2 <- face g hop

  removeTip g hprev
  removeTip g gprev

  (unless ?? removeFace g f2) =<< isBorder g hop
  fnull <- isBorder g h

  let
    worker hx = when (hx /= gprev) $ do
      n <- next g hx
      setFace g n f
      worker n
  worker hprev

  unless fnull (setHalfedgeF g f hprev)
  (setHalfedgeV g ?? hprev) =<< target g hprev
  (setHalfedgeV g ?? gprev) =<< target g gprev

  removeEdge g =<< edge g h
  return hprev

splitFace :: Monad m
          => Eq (Halfedge g)
          => MutableHalfedgeGraph m g
          => MutableFaceGraph m g
          => g
          -> Halfedge g
          -> Halfedge g
          -> m (Halfedge g)
splitFace g h1 h2 = do
  hnew <- halfedgeE g =<< addEdge g
  fnew <- addFace g
  insertTip g hnew h2
  (insertTip g ?? h1) =<< opposite g hnew
  setFace g hnew =<< face g h1
  (setFaceInFaceLoop g ?? fnew) =<< opposite g hnew
  (setHalfedgeF g ?? hnew) =<< face g hnew
  hnew' <- opposite g hnew
  (setHalfedgeF g ?? hnew') =<< face g hnew'
  return hnew
