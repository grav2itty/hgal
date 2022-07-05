module Hgal.Graph.Helpers where

import Hgal.Graph.Class


exactNumVertices :: HalfedgeGraph g
                 => g
                 -> Int
exactNumVertices =  length . vertices

exactNumHalfedges :: HalfedgeGraph g
                  => g
                  -> Int
exactNumHalfedges =  length . halfedges

exactNumEdges :: HalfedgeGraph g
              => g
              -> Int
exactNumEdges =  length . edges

exactNumFaces :: FaceGraph g
              => g
              -> Int
exactNumFaces = length . faces

