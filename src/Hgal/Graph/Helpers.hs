module Hgal.Graph.Helpers where

import Hgal.Graph.Class

next2 :: HalfedgeGraph g v h e
      => g -> h -> h
next2 g = next g . next g

next3 :: HalfedgeGraph g v h e
      => g -> h -> h
next3 g = next g . next g . next g

exactNumVertices :: HalfedgeGraph g v h e
                 => g -> Int
exactNumVertices = length . vertices

exactNumHalfedges :: HalfedgeGraph g v h e
                  => g -> Int
exactNumHalfedges = length . halfedges

exactNumEdges :: HalfedgeGraph g v h e
              => g -> Int
exactNumEdges = length . edges

exactNumFaces :: FaceGraph g v h e f
              => g -> Int
exactNumFaces = length . faces

