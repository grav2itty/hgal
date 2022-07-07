{-# LANGUAGE MultiWayIf #-}

module Hgal.Graph.EulerOperationsM where

import Control.Exception
import Control.Lens (both, findMOf, forMOf_, traversed, (??))
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Containers.ListUtils
import Data.Foldable
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Hgal.Graph.ClassM hiding (addEdge, addFace)
import qualified Hgal.Graph.ClassM as Graph (addEdge, addFace)
import Hgal.Graph.HelpersM
import Hgal.Graph.LoopsM

-- import Debug.Trace


addFace :: Foldable t
        => MutableHalfedgeGraph m g v h e
        => MutableFaceGraph m g v h e f
        => (Ord v, Eq h, Eq f)
        => g -> t v -> m f
addFace g vs = do
  let
    n = length vs
    indices = [0..n-1]
    indicesTuple = zip ic (drop 1 ic)
      where ic = take (n+1) . cycle $ indices
    vList = toList vs
    vertices = V.fromList vList

  uvs <- forM indicesTuple
           (\(i, ii) -> halfedgeVV (vertices V.! i) (vertices V.! ii))
  nullF <- nullFace g
  nullH <- nullHalfedge g

  let
    (halfedges, isNew) = V.unzip . V.fromList $ maybe (nullH, True) (, False) <$> uvs

    checkVertexCount = if n <= 2 then Nothing else Just ()
    checkVerticesU = if nubOrd vList /= vList then Nothing else Just ()
    checkVertex v = do
      p1 <- isIsolated g v
      p2 <- isBorder v
      return $ if p1 || p2 then Just () else Nothing
    checkHalfedge i = do
      p <- isBorder h
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
          p <- lift $ next innerPrev
          if p == innerNext then return []
            else do
              outerPrev <- lift $ opposite innerNext
              outerNext <- lift $ opposite innerPrev
              let
                worker hx = do
                  n <- lift $ (opposite <=< next) hx
                  isBorder <- lift $ isBorder n
                  if not isBorder || (n == innerPrev) then worker n
                    else return $ assert isBorder n
              borderPrev <- worker outerPrev
              borderNext <- lift $ do
                p <- next borderPrev
                isB <- isBorder p
                return $ assert isB p
              if borderNext == innerNext then mzero
                else do
                  patchStart <- lift $ next innerPrev
                  patchEnd <- lift $ prev innerNext
                  return [ (borderPrev, patchStart), (patchEnd, borderNext)
                         , (innerPrev, innerNext) ]

    createMissingEdges = forM indicesTuple $ \(i, ii) -> do
      when (isNew V.! i) $ do
        ne <- lift $ addEdge g (vertices V.! i) (vertices V.! ii)
        he <- lift $ do
          h <- halfedge ne
          s <- source h
          return $ assert (h /= nullH && s == vertices V.! i) h
        modify (V.modify (\vec -> VM.write vec i he))
        lift $ (setFace ?? nullF) =<< opposite he

    setupHalfedgesF halfedges' (i, ii) = do
      let
        v = vertices V.! ii
        innerPrev = halfedges' V.! i
        innerNext = halfedges' V.! ii

      if bothOld (i, ii) then return []
        else do
          outerPrev <- opposite innerNext
          outerNext <- opposite innerPrev
          let innerLink = (innerPrev, innerNext)
          if
            | (isNew V.! i) && (isNew V.! ii) -> do
              hv <- halfedge v
              nullH <- nullHalfedge g
              bh <- isBorder hv
              hv' <- if hv == nullH || bh then return hv
                else do
                  hAroundV <- halfedgesAroundTarget hv
                  r <- findMOf traversed isBorder hAroundV
                  return $ fromMaybe nullH r
              if hv' == nullH
                then do
                  setHalfedge v outerPrev
                  return [innerLink, (outerPrev, outerNext)]
                else do
                  let borderPrev' = hv'
                  borderNext' <- next borderPrev'
                  return [innerLink, (borderPrev', outerNext), (outerPrev, borderNext')]
            | (isNew V.! i) -> do
              borderPrev <- prev innerNext
              setHalfedge v borderPrev
              return [innerLink, (borderPrev, outerNext)]
            | (isNew V.! ii) -> do
              borderNext <- next innerPrev
              setHalfedge v outerPrev
              return [innerLink, (outerPrev, borderNext)]


  r <- runMaybeT $ allCheck >> reLink
  case r of
    Nothing -> return nullF
    Just nextCache -> execStateT createMissingEdges halfedges >>= \halfedges' ->
      do
        f <- Graph.addFace g
        setHalfedge f (halfedges' V.! (n - 1))
        nextCache2 <- forM indicesTuple (setupHalfedgesF halfedges')
        forM_ indicesTuple (\(i, _) -> setFace (halfedges' V.! i) f)
        forMOf_ (both.traversed.traversed) (nextCache, nextCache2) (uncurry setNext)
        forM_ vertices (adjustIncomingHalfedge g)
        return f

addEdge :: MutableHalfedgeGraph m g v h e
        => g -> v -> v -> m e
addEdge g s t = do
  e <- Graph.addEdge g
  (setTarget ?? t) =<< halfedge e
  (setTarget ?? s) =<< (opposite <=< halfedge) e
  return e

splitVertex :: MutableHalfedgeGraph m g v h e
            => MutableFaceGraph m g v h e f
            => Eq h
            => g -> h -> h -> m h
splitVertex g h1 h2 = do
  -- add missing assert here

  hnew <- halfedge =<< Graph.addEdge g
  hnewopp <- opposite hnew
  vnew <- addVertex g
  insertHalfedge hnew h2
  insertHalfedge hnewopp h1
  setTarget hnew =<< target h1

  let
    worker hx = do
      setTarget hx vnew
      n <- (opposite <=< next) hx
      if n /= hnewopp
        then worker n
        else return n
  hnewopp' <- worker hnewopp

  setVertexHalfedge hnew
  setVertexHalfedge hnewopp'
  return hnew

joinVertex :: MutableHalfedgeGraph m g v h e
           => MutableFaceGraph m g v h e f
           => (Eq v, Eq h)
           => g -> h -> m h
joinVertex g h = do
  hop <- opposite h
  hprev <- prev hop
  gprev <- prev h
  hnext <- next hop
  gnext <- next h
  v_to_remove <- target hop
  v <- target h

  -- add missing assert here

  halfedgeAroundTarget g (\_ hx -> do
                             t <- target hx
                             assert (t == v_to_remove) $
                               setTarget hx v
                         ) hop

  setNext hprev hnext
  setNext gprev gnext
  setHalfedge v gprev

  bg <- isBorder gprev
  unless bg $ do
    (setHalfedge ?? gprev) =<< face gprev

  bh <- isBorder hprev
  unless bh $ do
    (setHalfedge ?? hprev) =<< face hprev

  remove =<< edge h
  remove v_to_remove

  return hprev

fillHole :: MutableHalfedgeGraph m g v h e
         => MutableFaceGraph m g v h e f
         => Eq h
         => g -> h -> m ()
fillHole g h = do
  f <- Graph.addFace g
  halfedgeAroundFace g (\_ h' -> setFace h' f) h
  setHalfedge f h

addCenterVertex :: MutableHalfedgeGraph m g v h e
                => MutableFaceGraph m g v h e f
                => Eq h
                => g -> h -> m h
addCenterVertex g h = do
  hnew <- halfedge =<< Graph.addEdge g
  vnew <- addVertex g
  closeTip hnew vnew

  (insertTip ?? h) =<< opposite hnew
  setFace hnew =<< face h
  (setHalfedge ?? h) =<< face h

  h2 <- next =<< opposite hnew
  let
    worker hx = do
      n <- next hx
      when (n /= hnew) $ do
        gnew <- halfedge =<< Graph.addEdge g
        insertTip gnew hnew
        (insertTip ?? hx) =<< opposite gnew
        fnew <- Graph.addFace g
        setFace hx fnew
        setFace gnew fnew
        (setFace ?? fnew) =<< next gnew
        (setHalfedge ?? hx) =<< face hx
        (worker <=< next <=< opposite) gnew
  worker h2

  nhnew <- next hnew
  setFace nhnew =<< face hnew
  setVertexHalfedge hnew
  return hnew

removeCenterVertex :: MutableHalfedgeGraph m g v h e
                   => MutableFaceGraph m g v h e f
                   => Eq h
                   => g -> h -> m h
removeCenterVertex g h = do
  h2 <- (opposite <=< next) h
  hret <- prev h
  let
    worker hx = when (hx /= h) $ do
      gprev <- prev hx
      setVertexHalfedge gprev
      removeTip gprev

      remove =<< face hx

      gnext <- (opposite <=< next) hx
      remove =<< edge hx
      worker gnext
  worker h2
  setVertexHalfedge hret
  removeTip hret
  remove =<< target h
  remove =<< edge h
  setFaceInFaceLoop hret =<< face hret
  (setHalfedge ?? hret) =<< face hret

  return hret

joinFace :: MutableHalfedgeGraph m g v h e
         => MutableFaceGraph m g v h e f
         => Eq h
         => g -> h -> m h
joinFace g h = do
  hop <- opposite h
  hprev <- prev h
  gprev <- prev hop
  f <- face h
  f2 <- face hop

  removeTip hprev
  removeTip gprev

  (unless ?? remove f2) =<< isBorder hop
  fnull <- isBorder h

  let
    worker hx = when (hx /= gprev) $ do
      n <- next hx
      setFace n f
      worker n
  worker hprev

  unless fnull (setHalfedge f hprev)
  (setHalfedge ?? hprev) =<< target hprev
  (setHalfedge ?? gprev) =<< target gprev

  remove =<< edge h
  return hprev

splitFace :: MutableHalfedgeGraph m g v h e
          => MutableFaceGraph m g v h e f
          => Eq h
          => g -> h -> h -> m h
splitFace g h1 h2 = do
  hnew <- halfedge =<< Graph.addEdge g
  fnew <- Graph.addFace g
  insertTip hnew h2
  (insertTip ?? h1) =<< opposite hnew
  setFace hnew =<< face h1
  (setFaceInFaceLoop ?? fnew) =<< opposite hnew
  (setHalfedge ?? hnew) =<< face hnew
  hnew' <- opposite hnew
  (setHalfedge ?? hnew') =<< face hnew'
  return hnew
