module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as M

addFace :: Foldable t
        => Eq (Halfedge g)
        => Eq (Face g)
        => Ord (Vertex g)
        => MutableHalfedgeGraph (State g) g
        => MutableFaceGraph (State g) g
        => g
        -> t (Vertex g)
        -> (Face g, g)
addFace g vs = runState (M.addFace g vs) g

addCenterVertex :: Eq (Halfedge g)
                => MutableHalfedgeGraph (State g) g
                => MutableFaceGraph (State g) g
                => g
                -> Halfedge g
                -> (Halfedge g, g)
addCenterVertex g h = runState (M.addCenterVertex g h) g
