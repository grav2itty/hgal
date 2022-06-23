module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as EOM

addCenterVertex :: Eq (Halfedge g)
                => MutableHalfedgeGraph (State g) g
                => MutableFaceGraph (State g) g
                => g
                -> Halfedge g
                -> (Halfedge g, g)
addCenterVertex g h = runState (EOM.addCenterVertex g h) g
