module Hgal.Graph.Generators where

import Control.Monad.State

import Hgal.Graph.ClassM
import qualified Hgal.Graph.GeneratorsM as M


makeTriangle :: MutableHalfedgeGraph (State g) g
             => MutableFaceGraph (State g) g
             => HasPoints (State g) g p
             => g
             -> p -> p -> p
             -> (Halfedge g, g)
makeTriangle g p0 p1 p2 = runState (M.makeTriangle g p0 p1 p2) g
