{-# LANGUAGE MultiWayIf #-}

module Hgal.Graph.EulerOperationsM where

import Control.Exception
import Control.Lens (both, findMOf, forMOf_, traversed, (??))
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Containers.ListUtils
import Data.Foldable
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Hgal.Graph.ClassM hiding (addFace, addEdge)
import qualified Hgal.Graph.ClassM as Graph (addFace, addEdge)
import Hgal.Graph.Helpers
import Hgal.Graph.LoopsM

import Debug.Trace


addFace :: Monad m
        => Foldable t
        => Eq (Halfedge g)
        => Eq (Face g)
        => Ord (Vertex g)
        => MutableHalfedgeGraph m g
        => MutableFaceGraph m g
        => g
        -> t (Vertex g)
        -> m (Face g)
addFace g vs = do
  let
    n = length vs
    indices = [0..n-1]
    indicesTuple = zip ic (drop 1 ic)
      where ic = take (n+1) . cycle $ indices
    vList = toList vs
    vertices = V.fromList vList

  uvs <- forM indicesTuple
           (\(i, ii) -> halfedgeVV g (vertices V.! i) (vertices V.! ii))
  nullF <- nullFace g
  nullH <- nullHalfedge g

  let
    (halfedges, isNew) = V.unzip . V.fromList $ maybe (nullH, True) (, False) <$> uvs

    checkVertexCount = if n <= 2 then Nothing else Just ()
    checkVerticesU = if nubOrd vList /= vList then Nothing else Just ()
    checkVertex v = do
      p1 <- isIsolated g v
      p2 <- isBorderV g v
      return $ if p1 || p2 then Just () else Nothing
    checkHalfedge i = do
      p <- isBorderH g h
      return $ if isNew V.! i || p then Just () else Nothing
        where h = halfedges V.! i
    allCheck = (MaybeT . pure $ checkVertexCount >> checkVerticesU) >>
               lift (forM vertices checkVertex) >>
               lift (forM indices checkHalfedge)

    bothOld (i, ii) = not $ (isNew V.! i) || (isNew V.! ii)

    reLink = forM indicesTuple $ \(i, ii) -> do
      if not $ bothOld (i, ii) then return []
        else do
          let
            innerPrev = halfedges V.! i
            innerNext = halfedges V.! ii
          p <- lift $ next g innerPrev
          if p == innerNext then return []
            else do
              outerPrev <- lift $ opposite g innerNext
              outerNext <- lift $ opposite g innerPrev
              let
                worker hx = do
                  n <- lift $ (opposite g <=< next g) hx
                  isBorder <- lift $ isBorderH g n
                  if not isBorder || (n == innerPrev) then worker n
                    else return $ assert isBorder n
              borderPrev <- worker outerPrev
              borderNext <- lift $ do
                p <- next g borderPrev
                isBorder <- isBorderH g p
                return $ assert isBorder p
              if borderNext == innerNext then mzero
                else do
                  patchStart <- lift $ next g innerPrev
                  patchEnd <- lift $ prev g innerNext
                  return [ (borderPrev, patchStart), (patchEnd, borderNext)
                         , (innerPrev, innerNext) ]

    createMissingEdges = forM indicesTuple $ \(i, ii) -> do
      when (isNew V.! i) $ do
        ne <- lift $ addEdge g (vertices V.! i) (vertices V.! ii)
        he <- lift $ do
          h <- halfedgeE g ne
          s <- source g h
          return $ assert (h /= nullH && s == vertices V.! i) h
        modify (V.modify (\vec -> VM.write vec i he))
        lift $ (setFace g ?? nullF) =<< opposite g he

    setupHalfedgesF halfedges' (i, ii) = do
      let
        v = vertices V.! ii
        innerPrev = halfedges' V.! i
        innerNext = halfedges' V.! ii

      if bothOld (i, ii) then return []
        else do
          outerPrev <- opposite g innerNext
          outerNext <- opposite g innerPrev
          let innerLink = (innerPrev, innerNext)
          if
            | (isNew V.! i) && (isNew V.! ii) -> do
              hv <- halfedgeV g v
              nullH <- nullHalfedge g
              bh <- isBorderH g hv
              hv' <- if hv == nullH || bh then return hv
                else do
                  hAroundV <- halfedgesAroundTarget g hv
                  r <- findMOf traversed (isBorderH g) hAroundV
                  return $ fromMaybe nullH r
              if hv' == nullH
                then do
                  setHalfedgeV g v outerPrev
                  return [innerLink, (outerPrev, outerNext)]
                else do
                  let borderPrev' = hv'
                  borderNext' <- next g borderPrev'
                  return [innerLink, (borderPrev', outerNext), (outerPrev, borderNext')]
            | (isNew V.! i) -> do
              borderPrev <- prev g innerNext
              setHalfedgeV g v borderPrev
              return [innerLink, (borderPrev, outerNext)]
            | (isNew V.! ii) -> do
              borderNext <- next g innerPrev
              setHalfedgeV g v outerPrev
              return [innerLink, (outerPrev, borderNext)]


  r <- runMaybeT $ allCheck >> reLink
  case r of
    Nothing -> return nullF
    Just nextCache -> execStateT createMissingEdges halfedges >>= \halfedges' ->
      do
        f <- Graph.addFace g
        setHalfedgeF g f (halfedges' V.! (n - 1))
        nextCache2 <- forM indicesTuple (setupHalfedgesF halfedges')
        forM_ indicesTuple (\(i, _) -> setFace g (halfedges' V.! i) f)
        forMOf_ (both.traversed.traversed) (nextCache, nextCache2) (uncurry (setNext g))
        forM_ vertices (adjustIncomingHalfedge g)
        return f

addEdge :: Monad m
        => MutableHalfedgeGraph m g
        => g
        -> Vertex g
        -> Vertex g
        -> m (Edge g)
addEdge g s t = do
  e <- Graph.addEdge g
  (setTarget g ?? t) =<< halfedgeE g e
  (setTarget g ?? s) =<< (opposite g <=< halfedgeE g) e
  return e

splitVertex :: Monad m
            => Eq (Halfedge g)
            => MutableHalfedgeGraph m g
            => MutableFaceGraph m g
            => g
            -> Halfedge g
            -> Halfedge g
            -> m (Halfedge g)
splitVertex g h1 h2 = do
  -- add missing assert here

  hnew <- halfedgeE g =<< Graph.addEdge g
  hnewopp <- opposite g hnew
  vnew <- addVertex g
  insertHalfedge g hnew h2
  insertHalfedge g hnewopp h1
  setTarget g hnew =<< target g h1

  let
    worker hx = do
      setTarget g hx vnew
      n <- (opposite g <=< next g) hx
      if n /= hnewopp
        then worker n
        else return n
  hnewopp' <- worker hnewopp

  setVertexHalfedge g hnew
  setVertexHalfedge g hnewopp'
  return hnew

joinVertex :: Monad m
           => Eq (Halfedge g)
           => Eq (Vertex g)
           => MutableHalfedgeGraph m g
           => MutableFaceGraph m g
           => g
           -> Halfedge g
           -> m (Halfedge g)
joinVertex g h = do
  hop <- opposite g h
  hprev <- prev g hop
  gprev <- prev g h
  hnext <- next g hop
  gnext <- next g h
  v_to_remove <- target g hop
  v <- target g h

  -- add missing assert here

  halfedgeAroundTarget g (\g' hx -> do
                             t <- target g hx
                             assert (t == v_to_remove) $
                               setTarget g' hx v
                         ) hop

  setNext g hprev hnext
  setNext g gprev gnext
  setHalfedgeV g v gprev

  bg <- isBorderH g gprev
  unless bg $ do
    (setHalfedgeF g ?? gprev) =<< face g gprev

  bh <- isBorderH g hprev
  unless bh $ do
    (setHalfedgeF g ?? hprev) =<< face g hprev

  removeEdge g =<< edge g h
  removeVertex g v_to_remove

  return hprev

addCenterVertex :: Monad m
                => Eq (Halfedge g)
                => MutableHalfedgeGraph m g
                => MutableFaceGraph m g
                => g
                -> Halfedge g
                -> m (Halfedge g)
addCenterVertex g h = do
  hnew <- halfedgeE g =<< Graph.addEdge g
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
        gnew <- halfedgeE g =<< Graph.addEdge g
        insertTip g gnew hnew
        (insertTip g ?? hx) =<< opposite g gnew
        fnew <- Graph.addFace g
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

  (unless ?? removeFace g f2) =<< isBorderH g hop
  fnull <- isBorderH g h

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
  hnew <- halfedgeE g =<< Graph.addEdge g
  fnew <- Graph.addFace g
  insertTip g hnew h2
  (insertTip g ?? h1) =<< opposite g hnew
  setFace g hnew =<< face g h1
  (setFaceInFaceLoop g ?? fnew) =<< opposite g hnew
  (setHalfedgeF g ?? hnew) =<< face g hnew
  hnew' <- opposite g hnew
  (setHalfedgeF g ?? hnew') =<< face g hnew'
  return hnew
