module Hgal.Graph.Helpers where

import Hgal.Graph.Class


exactNumVertices :: HalfedgeGraph g v h e
                 => g -> Int
exactNumVertices =  length . vertices

exactNumHalfedges :: HalfedgeGraph g v h e
                  => g -> Int
exactNumHalfedges =  length . halfedges

exactNumEdges :: HalfedgeGraph g v h e
              => g -> Int
exactNumEdges =  length . edges

exactNumFaces :: FaceGraph g v h e f
              => g -> Int
exactNumFaces = length . faces

