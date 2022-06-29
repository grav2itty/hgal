module Hgal.Graph.Loops where

import Control.Monad.State
import Data.Vector.Circular (CircularVector)

import Hgal.Graph.ClassM
import Hgal.Graph.LoopsM as M

-------------------------------------------------------------------------------
-- perform action on each element

halfedgeAroundTarget :: Eq (Halfedge g)
                     => HalfedgeGraph (State g) g
                     => g
                     -> (g -> Halfedge g -> (State g) a)
                     -> Halfedge g
                     -> a
halfedgeAroundTarget g f h = evalState (M.halfedgeAroundTarget g f h) g

-------------------------------------------------------------------------------
-- gets circular vector of all elements

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

facesAroundTarget :: Eq (Halfedge g)
                  => FaceGraph (State g) g
                  => g
                  -> Halfedge g
                  -> CircularVector (Face g)
facesAroundTarget g h = evalState (M.facesAroundTarget g h) g

facesAroundFace :: Eq (Halfedge g)
                => FaceGraph (State g) g
                => g
                -> Halfedge g
                -> CircularVector (Face g)
facesAroundFace g h = evalState (M.facesAroundFace g h) g

verticesAroundTarget :: Eq (Halfedge g)
                     => HalfedgeGraph (State g) g
                     => g
                     -> Halfedge g
                     -> CircularVector (Vertex g)
verticesAroundTarget g h = evalState (M.verticesAroundTarget g h) g

verticesAroundFace :: Eq (Halfedge g)
                   => HalfedgeGraph (State g) g
                   => g
                   -> Halfedge g
                   -> CircularVector (Vertex g)
verticesAroundFace g h = evalState (M.verticesAroundFace g h) g

edgesAroundFace :: Eq (Halfedge g)
                => HalfedgeGraph (State g) g
                => g
                -> Halfedge g
                -> CircularVector (Edge g)
edgesAroundFace g h = evalState (M.edgesAroundFace g h) g
