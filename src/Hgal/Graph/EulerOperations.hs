module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.EulerOperationsM as EulerM

addFace :: Foldable t
        => Eq (Halfedge g)
        => Eq (Face g)
        => Ord (Vertex g)
        => Halfedge g ~ M.Halfedge g
        => Face g ~ M.Face g
        => Vertex g ~ M.Vertex g
        => M.MutableHalfedgeGraph (State g) g
        => M.MutableFaceGraph (State g) g
        => g
        -> t (Vertex g)
        -> (Face g, g)
addFace g vs = runState (EulerM.addFace g vs) g

addCenterVertex :: Eq (Halfedge g)
                => Halfedge g ~ M.Halfedge g
                => M.MutableHalfedgeGraph (State g) g
                => M.MutableFaceGraph (State g) g
                => g
                -> Halfedge g
                -> (Halfedge g, g)
addCenterVertex g h = runState (EulerM.addCenterVertex g h) g

joinFace :: Eq (Halfedge g)
         => Halfedge g ~ M.Halfedge g
         => M.MutableHalfedgeGraph (State g) g
         => M.MutableFaceGraph (State g) g
         => g
         -> Halfedge g
         -> (Halfedge g, g)
joinFace g h = runState (EulerM.joinFace g h) g
