module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as EulerM

addFace :: Foldable t
        => MutableFaceGraph (State g) g v h e f
        => (Ord v, Eq h, Eq f)
        => g -> t v -> (f, g)
addFace g vs = runState (EulerM.addFace g vs) g

removeFace :: MutableFaceGraph (State g) g v h e f
           => Eq h
           => g -> h -> g
removeFace g h = execState (EulerM.removeFace g h) g

addEdge :: MutableHalfedgeGraph (State g) g v h e
        => g -> v -> v -> (e, g)
addEdge g v1 v2 = runState (EulerM.addEdge g v1 v2) g

splitEdge :: MutableFaceGraph (State g) g v h e f
          => Eq h
          => g -> h -> (h, g)
splitEdge g h = runState (EulerM.splitEdge g h) g

joinLoop :: MutableFaceGraph (State g) g v h e f
         => (Eq h, Eq f)
         => g -> h -> h -> (h, g)
joinLoop g h1 h2 = runState (EulerM.joinLoop g h1 h2) g

splitLoop :: MutableFaceGraph (State g) g v h e f
          => (Eq v, Eq h)
          => g -> h -> h -> h -> (h, g)
splitLoop g h i j = runState (EulerM.splitLoop g h i j) g

splitVertex :: MutableFaceGraph (State g) g v h e f
            => Eq h
            => g -> h -> h -> (h, g)
splitVertex g h1 h2 = runState (EulerM.splitVertex g h1 h2) g

joinVertex :: MutableFaceGraph (State g) g v h e f
           => (Eq v, Eq h)
           => g -> h -> (h, g)
joinVertex g h = runState (EulerM.joinVertex g h) g

makeHole :: MutableFaceGraph (State g) g v h e f
         => Eq h
         => g -> h -> g
makeHole g h = execState (EulerM.makeHole g h) g

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

addVertexAndFaceToBorder :: MutableFaceGraph (State g) g v h e f
                         => Eq h
                         => g -> h -> h -> (h, g)
addVertexAndFaceToBorder g h1 h2 = runState (EulerM.addVertexAndFaceToBorder g h1 h2) g

addFaceToBorder :: MutableFaceGraph (State g) g v h e f
                => Eq h
                => g -> h -> h -> (h, g)
addFaceToBorder g h1 h2 = runState (EulerM.addFaceToBorder g h1 h2) g

joinFace :: MutableFaceGraph (State g) g v h e f
         => Eq h
         => g -> h -> (h, g)
joinFace g h = runState (EulerM.joinFace g h) g

splitFace :: MutableFaceGraph (State g) g v h e f
          => Eq h
          => g -> h -> h -> (h, g)
splitFace g h1 h2 = runState (EulerM.splitFace g h1 h2) g
