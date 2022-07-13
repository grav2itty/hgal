module Hgal.Graph.Loops where

import Control.Monad.State
import Data.Vector.Circular (CircularVector)

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import qualified Hgal.Graph.LoopsM as M


nextAroundTarget :: HalfedgeGraph g v h e
                 => g -> h -> h
nextAroundTarget g = opposite g . next g

prevAroundTarget :: HalfedgeGraph g v h e
                 => g -> h -> h
prevAroundTarget g = prev g . opposite g

nextAroundSource :: HalfedgeGraph g v h e
                 => g -> h -> h
nextAroundSource g = next g . opposite g

prevAroundSource :: HalfedgeGraph g v h e
                 => g -> h -> h
prevAroundSource g = opposite g . prev g


distance :: HalfedgeGraph g v h e
         => Eq h
         => g -> (g -> h -> h) -> h -> h -> Maybe Int
distance g f h1 h2 = worker h1 0
  where
    worker hx d =
      let n = f g hx
      in if n == h1 then Nothing
           else if n == h2 then Just (d + 1) else worker n (d + 1)


-- nextAroundTarget :: HalfedgeGraph g
--                  => g
--                  -> Halfedge g
--                  -> Halfedge g
-- nextAroundTarget g = opposite g . next g

-- halfedgeAroundTarget :: Eq (Halfedge g)
--                      => HalfedgeGraph g
--                      => g
--                      -> (g -> Halfedge g -> Maybe (g, a))
--                      -> Halfedge g
--                      -> (g, [a])
-- halfedgeAroundTarget = loop nextAroundTarget

-- loop :: Eq (Halfedge g)
--      => (g -> Halfedge g -> Halfedge g)
--      -> g
--      -> (g -> Halfedge g -> Maybe (g, a))
--      -> Halfedge g
--      -> (g, [a])
-- loop m g f h = worker g h []
--   where
--     worker gx hx ls =
--       let n = m gx hx
--       in case f gx hx of
--            Nothing -> (gx, ls)
--            Just (gx', a) -> if n == h then (gx', a:ls)
--                               else worker gx' n (a:ls)

-------------------------------------------------------------------------------
-- gets circular vector of all elements

halfedgesAroundTarget :: M.HalfedgeGraph (State g) g v h e
                      => Eq h
                      => g -> h -> CircularVector h
halfedgesAroundTarget g h = evalState (M.halfedgesAroundTarget h) g

halfedgesAroundSource :: M.HalfedgeGraph (State g) g v h e
                      => Eq h
                      => g -> h -> CircularVector h
halfedgesAroundSource g h = evalState (M.halfedgesAroundSource h) g

halfedgesAroundFace :: M.HalfedgeGraph (State g) g v h e
                    => Eq h
                    => g -> h -> CircularVector h
halfedgesAroundFace g h = evalState (M.halfedgesAroundFace h) g

facesAroundTarget :: M.FaceGraph (State g) g v h e f
                  => Eq h
                  => g -> h -> CircularVector f
facesAroundTarget g h = evalState (M.facesAroundTarget h) g

facesAroundFace :: M.FaceGraph (State g) g v h e f
                => Eq h
                => g -> h -> CircularVector f
facesAroundFace g h = evalState (M.facesAroundFace h) g

verticesAroundTarget :: M.HalfedgeGraph (State g) g v h e
                     => Eq h
                     => g -> h -> CircularVector v
verticesAroundTarget g h = evalState (M.verticesAroundTarget h) g

verticesAroundFace :: M.HalfedgeGraph (State g) g v h e
                   => Eq h
                   => g -> h -> CircularVector v
verticesAroundFace g h = evalState (M.verticesAroundFace h) g

edgesAroundFace :: M.HalfedgeGraph (State g) g v h e
                => Eq h
                => g -> h -> CircularVector e
edgesAroundFace g h = evalState (M.edgesAroundFace h) g
