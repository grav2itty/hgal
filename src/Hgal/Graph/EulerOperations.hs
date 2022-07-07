module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as EulerM

addFace :: Foldable t
        => MutableHalfedgeGraph (State g) g v h e
        => MutableFaceGraph (State g) g v h e f
        => (Ord v, Eq h, Eq f)
        => g -> t v -> (f, g)
addFace g vs = runState (EulerM.addFace g vs) g

addCenterVertex :: MutableHalfedgeGraph (State g) g v h e
                => MutableFaceGraph (State g) g v h e f
                => Eq h
                => g -> h -> (h, g)
addCenterVertex g h = runState (EulerM.addCenterVertex g h) g

joinFace :: MutableHalfedgeGraph (State g) g v h e
         => MutableFaceGraph (State g) g v h e f
         => Eq h
         => g -> h -> (h, g)
joinFace g h = runState (EulerM.joinFace g h) g
