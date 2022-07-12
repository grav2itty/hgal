module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as EulerM

addFace :: Foldable t
        => MutableFaceGraph (State g) g v h e f
        => (Ord v, Eq h, Eq f)
        => g -> t v -> (f, g)
addFace g vs = runState (EulerM.addFace g vs) g

addEdge :: MutableHalfedgeGraph (State g) g v h e
        => g -> v -> v -> (e, g)
addEdge g v1 v2 = runState (EulerM.addEdge g v1 v2) g

splitVertex :: MutableFaceGraph (State g) g v h e f
            => Eq h
            => g -> h -> h -> (h, g)
splitVertex g h1 h2 = runState (EulerM.splitVertex g h1 h2) g

joinVertex :: MutableFaceGraph (State g) g v h e f
           => (Eq v, Eq h)
           => g -> h -> (h ,g)
joinVertex g h = runState (EulerM.joinVertex g h) g

fillHole :: MutableFaceGraph (State g) g v h e f
         => Eq h
         => g -> h -> g
fillHole g h = execState (EulerM.fillHole g h) g

addCenterVertex :: MutableFaceGraph (State g) g v h e f
                => Eq h
                => g -> h -> (h, g)
addCenterVertex g h = runState (EulerM.addCenterVertex g h) g

removeCenterVertex :: MutableFaceGraph (State g) g v h e f
                   => Eq h
                   => g -> h -> (h, g)
removeCenterVertex g h = runState (EulerM.removeCenterVertex g h) g

joinFace :: MutableFaceGraph (State g) g v h e f
         => Eq h
         => g -> h -> (h, g)
joinFace g h = runState (EulerM.joinFace g h) g

splitFace :: MutableFaceGraph (State g) g v h e f
          => Eq h
          => g -> h -> h -> (h, g)
splitFace g h1 h2 = runState (EulerM.splitFace g h1 h2) g
