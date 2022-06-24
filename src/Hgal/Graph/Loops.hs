module Hgal.Graph.Loops where

import Control.Monad.State
import Data.Vector.Circular (CircularVector)

import Hgal.Graph.ClassM
import Hgal.Graph.LoopsM as M

halfedgesAroundTarget :: Eq (Halfedge g)
                      => HalfedgeGraph (State g) g
                      => g
                      -> Halfedge g
                      -> CircularVector (Halfedge g)
halfedgesAroundTarget g h = evalState (M.halfedgesAroundTarget g h) g

halfedgesAroundSource :: Eq (Halfedge g)
                      => HalfedgeGraph (State g) g
                      => g
                      -> Halfedge g
                      -> CircularVector (Halfedge g)
halfedgesAroundSource g h = evalState (M.halfedgesAroundSource g h) g

halfedgesAroundFace :: Eq (Halfedge g)
                    => HalfedgeGraph (State g) g
                    => g
                    -> Halfedge g
                    -> CircularVector (Halfedge g)
halfedgesAroundFace g h = evalState (M.halfedgesAroundFace g h) g
