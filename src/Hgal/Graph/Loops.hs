module Hgal.Graph.Loops where

import Control.Monad.State
import Data.Vector.Circular (CircularVector)

import Hgal.Graph.Class

import qualified Hgal.Graph.ClassM as M
import qualified Hgal.Graph.LoopsM as M


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

halfedgesAroundTarget :: Eq (M.Halfedge g)
                      => M.HalfedgeGraph (State g) g
                      => g
                      -> M.Halfedge g
                      -> CircularVector (M.Halfedge g)
halfedgesAroundTarget g h = evalState (M.halfedgesAroundTarget g h) g

halfedgesAroundSource :: Eq (M.Halfedge g)
                      => M.HalfedgeGraph (State g) g
                      => g
                      -> M.Halfedge g
                      -> CircularVector (M.Halfedge g)
halfedgesAroundSource g h = evalState (M.halfedgesAroundSource g h) g

halfedgesAroundFace :: Eq (M.Halfedge g)
                    => M.HalfedgeGraph (State g) g
                    => g
                    -> M.Halfedge g
                    -> CircularVector (M.Halfedge g)
halfedgesAroundFace g h = evalState (M.halfedgesAroundFace g h) g

facesAroundTarget :: Eq (M.Halfedge g)
                  => M.FaceGraph (State g) g
                  => g
                  -> M.Halfedge g
                  -> CircularVector (M.Face g)
facesAroundTarget g h = evalState (M.facesAroundTarget g h) g

facesAroundFace :: Eq (M.Halfedge g)
                => M.FaceGraph (State g) g
                => g
                -> M.Halfedge g
                -> CircularVector (M.Face g)
facesAroundFace g h = evalState (M.facesAroundFace g h) g

verticesAroundTarget :: Eq (M.Halfedge g)
                     => M.HalfedgeGraph (State g) g
                     => g
                     -> M.Halfedge g
                     -> CircularVector (M.Vertex g)
verticesAroundTarget g h = evalState (M.verticesAroundTarget g h) g

verticesAroundFace :: Eq (M.Halfedge g)
                   => M.HalfedgeGraph (State g) g
                   => g
                   -> M.Halfedge g
                   -> CircularVector (M.Vertex g)
verticesAroundFace g h = evalState (M.verticesAroundFace g h) g

edgesAroundFace :: Eq (M.Halfedge g)
                => M.HalfedgeGraph (State g) g
                => g
                -> M.Halfedge g
                -> CircularVector (M.Edge g)
edgesAroundFace g h = evalState (M.edgesAroundFace g h) g
