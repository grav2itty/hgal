module Hgal.Graph.Predicates where

import Control.Lens
import Control.Monad.State
import Data.Bifunctor
import Data.Either

import Hgal.Graph.Class
import Hgal.Graph.Helpers
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.Loops

isValidHalfedgeGraph :: HalfedgeGraph g v h e
                     => M.HalfedgeGraph (State g) g v h e
                     => (Eq v, Eq h, Show v, Show h)
                     => g -> Either String Bool
isValidHalfedgeGraph g = collectErrors [hsI, vsI, countHalfedges, geometryI]
  where
    hsI = second and (mapM (halfedgeIntegrity g) $ halfedges g)
    vsI = second and (mapM (vertexIntegrity g) $ vertices g)
    countHalfedges
      | lengthOf (folded.to (halfedgesAroundTarget g . halfedge g).folded)
        (vertices g) > exactNumHalfedges g = Left "Too many halfedges around vertex."
      |  otherwise = Right True
    geometryI = second and (mapM (halfedgeG g) $ halfedges g)
      where
        halfedgeG g h
          | (next g h == h) || (target g h == target g (opposite g h)) = Left "Pointer validity corrupted."
          | otherwise = Right True

isValidFaceGraph :: FaceGraph g v h e f
                 => M.FaceGraph (State g) g v h e f
                 => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                 => g -> Either String Bool
isValidFaceGraph g = collectErrors [isValidHalfedgeGraph g, fsI, countCheck, halfedgesI]
  where
    fsI = second and (mapM (faceIntegrity g) $ faces g)
    countHalfedgesAroundFaces =
      lengthOf (folded.to (halfedgesAroundFace g . halfedge g).folded) (faces g)
    countBorderHalfedges = length . filter (isBorder g) . halfedges $ g
    countCheck
      | countHalfedgesAroundFaces + countBorderHalfedges == exactNumHalfedges g = Right True
      | otherwise = Left "Counting halfedges via faces failed."
    halfedgesI = second and (mapM (halfedgeF g) $ halfedges g)
      where
        halfedgeF g h
          | face g h /= face g (next g h) = Left $ "Halfedge " ++ show h ++ " and its next have a differet face."
          | otherwise = Right True

isValidPolygonMesh :: FaceGraph g v h e f
                   => M.FaceGraph (State g) g v h e f
                   => (Eq v, Eq h, Eq f, Show v, Show h, Show f)
                   => g -> Either String Bool
isValidPolygonMesh g = collectErrors [isValidFaceGraph g]

vertexIntegrity :: HalfedgeGraph g v h e
                => (Eq v, Show v)
                => g -> v -> Either String Bool
vertexIntegrity g v
  | fromRight False (isValid g h) && target g (halfedge g v) == v = Right True
  | otherwise = Left $ "Halfedge of vertex " ++ show v ++ " is not an incoming halfedge.\n"
  where h = halfedge g v

halfedgeIntegrity :: HalfedgeGraph g v h e
                  => (Eq v, Eq h, Show h)
                  => g -> h -> Either String Bool
halfedgeIntegrity g h = halfedgeI >> edgeI >> oppositeI >> previousI >> nextI >> vertexI >> nextOppositeI
  where
    halfedgeI =
      first (\s -> "Integrity of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid g (next g h), isValid g (opposite g h)]
    edgeI
      | halfedge g (edge g h) == h = Right True
      | otherwise = Left $ "Integrity of edge of " ++ show h ++ " corrupted.\n"
    oppositeI
      | opposite g h /= h && opposite g (opposite g h) == h = Right True
      | otherwise = Left $ "Integrity of opposite halfedge of " ++ show h ++ " corrupted.\n"
    previousI
      | next g (prev g h) == h = Right True
      | otherwise = Left $ "Integrity of previous halfedge of " ++ show h ++ " corrupted.\n"
    nextI
      | prev g (next g h) == h = Right True
      | otherwise = Left $ "Integrity of next halfedge of " ++ show h ++ " corrupted.\n"
    vertexI =
      first (\s -> "Integrity of vertex of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid g (target g h)]
    nextOppositeI
      | target g (opposite g (next g h)) == target g h = Right True
      | otherwise = Left $ "Halfedge vertex of next opposite is not the same for " ++ show h ++ ".\n"

faceIntegrity :: FaceGraph g v h e f
              => (Eq f, Show f)
              => g -> f -> Either String Bool
faceIntegrity g f = faceI
  where
    faceI
      | fromRight False (isValid g h) && face g h == f = Right True
      | otherwise = Left $ "Halfedge of face " ++ show f ++ " points to different face.\n"
      where h = halfedge g f


collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left
