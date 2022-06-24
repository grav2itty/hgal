{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hgal.Data.SurfaceMesh where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Either
import Data.Foldable (length)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence hiding (empty, length)
import qualified Data.Sequence as Seq (empty)

import qualified Hgal.Graph.Class as Graph
import qualified Hgal.Graph.ClassM as GraphM
import Hgal.Graph.Loops

import Debug.Trace
import qualified Hgal.Graph.EulerOperationsM as Euler


data family Connectivity a :: *

class Element a where
  data Size a :: *

  name :: a -> String

  numElements :: SurfaceMesh v h e f -> Size a
  new :: SurfaceMesh v h e f -> a

  nullE :: a
  default nullE :: Bounded a => a
  nullE = maxBound

  conn :: a -> Lens' (SurfaceMesh v h e f) (Connectivity a)

  hasValidIndex :: SurfaceMesh v h e f -> a -> Bool
  hasValidConnectivity :: SurfaceMesh v h e f -> a -> Either String Bool

  isValid :: SurfaceMesh v h e f -> a -> Either String Bool
  isValid sm v
    | hasValidIndex sm v = hasValidConnectivity sm v
    | otherwise = Right False

  isBorder :: SurfaceMesh v h e f -> a -> Bool

  isRemoved :: SurfaceMesh v h e f -> a -> Bool
  isRemoved _ _ = False


  vertex :: SurfaceMesh v h e f -> a -> Vertex
  vertex sm = vertex sm . halfedge sm

  halfedge :: SurfaceMesh v h e f -> a -> Halfedge

  setHalfedge :: SurfaceMesh v h e f -> a -> Halfedge -> SurfaceMesh v h e f
  setHalfedge sm _ _ = sm

  edge :: SurfaceMesh v h e f -> a -> Edge
  edge sm = (\(Halfedge i) -> Edge i) . halfedge sm

  face :: SurfaceMesh v h e f -> a -> Face
  face sm = face sm . halfedge sm

  next :: SurfaceMesh v h e f -> a -> a
  prev :: SurfaceMesh v h e f -> a -> a


newtype Vertex = Vertex Int
    deriving (Bounded, Enum, Eq, Ord)
    deriving newtype (Bits, Integral, Num, Real)
newtype Halfedge = Halfedge Int
    deriving (Bounded, Enum, Eq, Ord)
    deriving newtype (Bits, Integral, Num, Real)
newtype Edge = Edge Int
    deriving (Bounded, Enum, Eq, Ord)
    deriving newtype (Bits, Integral, Num, Real)
newtype Face = Face Int
    deriving (Bounded, Enum, Eq, Ord)
    deriving newtype (Bits, Integral, Num, Real)

data SurfaceMesh v h e f = SurfaceMesh
  { _vconn :: Seq (Connectivity Vertex)
  , _hconn :: Seq (Connectivity Halfedge)
  , _fconn :: Seq (Connectivity Face)
  , _vprops :: Seq v
  , _hprops :: Map Halfedge h
  , _eprops :: Map Edge e
  , _fprops :: Map Face f
  }

makeLenses ''SurfaceMesh


newtype instance Connectivity Vertex = VertexConnectivity
  { _vH :: Halfedge }

data instance Connectivity Halfedge = HalfedgeConnectivity
  { _hF :: Face
  , _hV :: Vertex
  , _hN :: Halfedge
  , _hP :: Halfedge
  }

newtype instance Connectivity Face = FaceConnectivity
  { _fH :: Halfedge }


instance Element Vertex where

  name _ = "Vertex"

  numElements = VertexSize . length . _vconn
  new = Vertex . length . _vconn

  conn (Vertex i) = vconn.singular (ix i)

  hasValidIndex sm (Vertex i) = i < n
    where (VertexSize n) = numElements sm
  hasValidConnectivity sm v
    | not (hasValidIndex sm h) || isRemoved sm h = Left $ conError sm v "" h
    | otherwise = Right True
    where h = halfedge sm v

  isBorder sm v
    | h == nullE = True
    | otherwise = any (isBorder sm) (halfedgesAroundTarget sm h)
    where h = halfedge sm v


  vertex _ v = v
  halfedge sm v = view (conn v.vH) sm
  setHalfedge sm v h = set (conn v.vH) h sm
  next sm = vertex sm . next sm . halfedge sm
  prev sm = vertex sm . prev sm . halfedge sm

  newtype Size Vertex = VertexSize Int
    deriving (Bounded, Enum, Eq, Ord, Show)
    deriving newtype (Bits, Integral, Num, Real)

instance Element Halfedge where

  name _ = "Halfedge"

  numElements = HalfedgeSize . length . _hconn
  new = Halfedge . length . _hconn

  conn (Halfedge i) = hconn.singular (ix i)

  hasValidIndex sm (Halfedge i) =
    let (HalfedgeSize n) = numElements sm
    in i < n

  hasValidConnectivity sm h = collectErrors [faceC, vertexC, nextC, prevC]
    where
      faceC
        | isBorder sm h = Right True
        | not (hasValidIndex sm f) || isRemoved sm f = Left $ conError sm h "" f
        | otherwise = Right True
      vertexC
        | not (hasValidIndex sm v) || isRemoved sm v = Left $ conError sm h "" v
        | otherwise = Right True
      nextC
        | not (hasValidIndex sm hn) || isRemoved sm hn = Left $ conError sm h "Next" hn
        | otherwise = Right True
      prevC
        | not (hasValidIndex sm hp) || isRemoved sm hp = Left $ conError sm h "Prev" hp
        | otherwise = Right True
      f = face sm h
      v = target sm h
      hn = next sm h
      hp = prev sm h

  isBorder sm h = not . fromRight False . isValid sm . face sm $ h

  halfedge _ h = h
  vertex sm h = view (conn h.hV) sm
  face sm h = view (conn h.hF) sm
  next sm h = view (conn h.hN) sm
  prev sm h = view (conn h.hP) sm

  newtype Size Halfedge = HalfedgeSize Int
    deriving (Bounded, Enum, Eq, Ord, Show)
    deriving newtype (Bits, Integral, Num, Real)

instance Element Edge where

  name _ = "Edge"

  numElements = EdgeSize . (`div` 2) . length . _hconn
  new = Edge . length . _hconn

  hasValidIndex sm (Edge i) = i < n
    where (EdgeSize n) = numElements sm

  isValid sm e
    | hasValidIndex sm e = collectErrors [isValid sm h, isValid sm (opposite sm h)]
    | otherwise = Right False
    where h = halfedge sm e

  isBorder sm e = isBorder sm h || isBorder sm (opposite sm h)
    where h = halfedge sm e

  halfedge _ (Edge i) = Halfedge i
  next sm = edge sm . next sm . halfedge sm
  prev sm = edge sm . prev sm . halfedge sm

  newtype Size Edge = EdgeSize Int
    deriving (Bounded, Enum, Eq, Ord, Show)
    deriving newtype (Bits, Integral, Num, Real)

instance Element Face where

  name _ = "Face"

  numElements = FaceSize . length . _fconn
  new = Face . length . _fconn

  conn (Face i) = fconn.singular (ix i)

  hasValidIndex sm (Face i) = i < n
    where (FaceSize n) = numElements sm

  hasValidConnectivity sm f
    | not (hasValidIndex sm f) || isRemoved sm f = Left $ conError sm f "" h
    | otherwise = Right True
    where h = halfedge sm f

  isBorder sm v
    | h == nullE = True
    | otherwise = any (isBorder sm) (halfedgesAroundFace sm h)
    where h = halfedge sm v

  halfedge sm f = view (conn f.fH) sm
  setHalfedge sm f h = set (conn f.fH) h sm
  face _ f = f

  newtype Size Face = FaceSize Int
    deriving (Bounded, Enum, Eq, Ord, Show)
    deriving newtype (Bits, Integral, Num, Real)


empty :: SurfaceMesh v h e f
empty = SurfaceMesh
   Seq.empty Seq.empty Seq.empty
   Seq.empty
   Map.empty Map.empty Map.empty

-------------------------------------------------------------------------------
-- Elements

vertices :: SurfaceMesh v h e f -> [Vertex]
vertices sm = Prelude.filter (not . isRemoved sm) $
              Vertex <$> [0..fromIntegral (numElements sm :: Size Vertex) - 1]

halfedges :: SurfaceMesh v h e f -> [Halfedge]
halfedges sm = Prelude.filter (not . isRemoved sm) $
               Halfedge <$> [0..fromIntegral (numElements sm :: Size Halfedge) - 1]

edges :: SurfaceMesh v h e f -> [Edge]
edges sm = Prelude.filter (not . isRemoved sm) $
           Edge . (* 2) <$> [0..fromIntegral (numElements sm :: Size Edge) - 1]

faces :: SurfaceMesh v h e f -> [Face]
faces sm = Prelude.filter (not . isRemoved sm) $
           Face <$> [0..fromIntegral (numElements sm :: Size Face) - 1]

-------------------------------------------------------------------------------
-- Adding elements

addVertex :: SurfaceMesh v h e f -> (Vertex, SurfaceMesh v h e f)
addVertex sm =
  let sm' = (vconn._Wrapped') %~ (snoc ?? VertexConnectivity nullE) $ sm
  in (new sm, sm')

addEdge :: SurfaceMesh v h e f -> (Edge, SurfaceMesh v h e f)
addEdge sm =
  let addh = (snoc ?? HalfedgeConnectivity nullE nullE nullE nullE)
      sm' = (hconn._Wrapped') %~ (addh . addh) $ sm
  in (new sm, sm')

addFace :: SurfaceMesh v h e f -> (Face, SurfaceMesh v h e f)
addFace sm =
  let sm' = (fconn._Wrapped') %~ (snoc ?? FaceConnectivity nullE) $ sm
  in (new sm, sm')

-------------------------------------------------------------------------------
-- Removin elements

removeVertex :: SurfaceMesh v h e f -> Vertex -> SurfaceMesh v h e f
removeVertex sm v = sm

removeEdge :: SurfaceMesh v h e f -> Edge -> SurfaceMesh v h e f
removeEdge sm v = sm

removeFace :: SurfaceMesh v h e f -> Face -> SurfaceMesh v h e f
removeFace sm v = sm

-------------------------------------------------------------------------------
-- Connectivity

target :: SurfaceMesh v h e f -> Halfedge -> Vertex
target sm h = view (conn h.hV) sm

setTarget :: SurfaceMesh v h e f -> Halfedge -> Vertex -> SurfaceMesh v h e f
setTarget sm h v = set (conn h.hV) v sm

setFace :: SurfaceMesh v h e f -> Halfedge -> Face -> SurfaceMesh v h e f
setFace sm h f = set (conn h.hF) f sm

setNextOnly :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setNextOnly sm h nh = set (conn h.hN) nh sm

setPrevOnly :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setPrevOnly sm h ph = set (conn h.hP) ph sm

setNext :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setNext sm h nh = setPrevOnly (setNextOnly sm h nh) nh h

opposite :: SurfaceMesh v h e f -> Halfedge -> Halfedge
opposite _ h = xor h 1

source :: SurfaceMesh v h e f -> Halfedge -> Vertex
source sm h = target sm (opposite sm h)


halfedgeVV :: SurfaceMesh v h e f -> Vertex -> Vertex -> Maybe Halfedge
halfedgeVV sm sour tar =
  assert (hasValidIndex sm sour && hasValidIndex sm tar) result
  where
    h = halfedge sm tar
    worker hx =
      let n = next sm (opposite sm hx)
      in
        if source sm hx == sour then Just hx
          else if n /= h then worker n
             else Nothing
    result = if not (fromRight False $ isValid sm h) then Nothing
      else worker h

-------------------------------------------------------------------------------
-- string & error helpers

showConnectivity :: (Show a, Foldable t) => String -> t a -> String
showConnectivity t s =
  t ++ " :: " ++ ifoldMapOf folded
  (\i a -> "\n(" ++ show i ++ ", " ++ show a ++ ")") s

conError :: (Element a, Element b, Show a, Show b)
         => SurfaceMesh v h e f -> a -> String -> b -> String
conError sm els s elt =
  name els ++ " " ++ show els ++ " conneciivity error. " ++
  s ++ " " ++ name elt ++ " " ++ show elt ++ " is " ++
  if hasValidIndex sm elt then "removed." else "invalid."

collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left

-------------------------------------------------------------------------------
-- Show instances

instance Show (Connectivity Vertex) where
  show s = "H " ++ show (_vH s)

instance Show (Connectivity Halfedge) where
  show s = "F " ++ show (_hF s)
         ++ ", V " ++ show (_hV s)
         ++ ", Next " ++ show (_hN s)
         ++ ", Prev " ++ show (_hP s)

instance Show (Connectivity Face) where
  show s = "H " ++ show (_fH s)

instance Show (SurfaceMesh v h e f) where
  show s = unlines [ showConnectivity "Vertices" (_vconn s)
                   , showConnectivity "Halfedges" (_hconn s)
                   , showConnectivity "Faces" (_fconn s)
                   ]

instance Show Vertex where
  show ii@(Vertex i)
    | ii == nullE = "N"
    | otherwise = show i

instance Show Halfedge where
  show ii@(Halfedge i)
    | ii == nullE = "N"
    | otherwise = show i

instance Show Edge where
  show ii@(Edge i)
    | ii == nullE = "N"
    | otherwise = show . (`div` 2) $ i

instance Show Face where
  show ii@(Face i)
    | ii == nullE = "N"
    | otherwise = show i

-------------------------------------------------------------------------------
-- Internal Connectivity Lenses

vH :: Lens' (Connectivity Vertex) Halfedge
vH g (VertexConnectivity v) = VertexConnectivity <$> g v

hF :: Lens' (Connectivity Halfedge) Face
hF g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity x v n p) <$> g f

hV :: Lens' (Connectivity Halfedge) Vertex
hV g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f x n p) <$> g v

hN :: Lens' (Connectivity Halfedge) Halfedge
hN g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f v x p) <$> g n

hP :: Lens' (Connectivity Halfedge) Halfedge
hP g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f v n x) <$> g p

fH :: Lens' (Connectivity Face) Halfedge
fH g (FaceConnectivity v) = FaceConnectivity <$> g v


-------------------------------------------------------------------------------
-- Graph isntances

type instance Graph.VertexDescriptor (SurfaceMesh v h e f) = Vertex
type instance Graph.HalfedgeDescriptor (SurfaceMesh v h e f) = Halfedge
type instance Graph.EdgeDescriptor (SurfaceMesh v h e f) = Edge

instance Graph.HalfedgeGraph (SurfaceMesh v h e f) where
  edge = edge
  halfedgeV = halfedge
  halfedgeE = halfedge
  halfedgeVV = halfedgeVV
  opposite = opposite
  source = source
  target = target
  next = next
  prev = prev

  isBorderH = isBorder
  isBorderV = isBorder
  nullHalfedge _ = nullE

instance Graph.MutableHalfedgeGraph (SurfaceMesh v h e f) where
  addVertex = addVertex
  removeVertex = removeVertex
  addEdge = addEdge
  removeEdge = removeEdge
  setTarget = setTarget
  setNext = setNext
  setHalfedgeV = setHalfedge

type instance Graph.FaceDescriptor (SurfaceMesh v h e f) = Face

instance Graph.FaceGraph (SurfaceMesh v h e f) where
  face = face
  halfedgeF = halfedge

  nullFace _ = nullE

instance Graph.MutableFaceGraph (SurfaceMesh v h e f) where
  addFace = addFace
  removeFace = removeFace
  setFace = setFace
  setHalfedgeF = setHalfedge

type instance GraphM.VertexDescriptor (SurfaceMesh v h e f) = Vertex
type instance GraphM.HalfedgeDescriptor (SurfaceMesh v h e f) = Halfedge
type instance GraphM.EdgeDescriptor (SurfaceMesh v h e f) = Edge

type St v h e f = State (SurfaceMesh v h e f)

-- instance GraphM.HalfedgeGraphS (St v h e f) (SurfaceMesh v h e f)

instance GraphM.HalfedgeGraph (St v h e f) (SurfaceMesh v h e f) where
  edge _ h = gets (`edge` h)
  halfedgeV _ h = gets (`halfedge` h)
  halfedgeE _ h = gets (`halfedge` h)
  halfedgeVV _ h v = gets (\s -> halfedgeVV s h v)
  opposite _ h = gets (`opposite` h)
  source _ h = gets (`source` h)
  target _ h = gets (`target` h)
  next _ h = gets (`next` h)
  prev _ h = gets (`prev` h)

  isBorderH _ h = gets (`isBorder` h)
  isBorderV _ h = gets (`isBorder` h)
  nullHalfedge _ = return nullE

  showM _ = gets show

instance GraphM.MutableHalfedgeGraph (St v h e f) (SurfaceMesh v h e f) where
  addVertex _ = state addVertex
  removeVertex _ v = modify (`removeVertex` v)
  addEdge _ = state addEdge
  removeEdge _ v = modify (`removeEdge` v)
  setTarget _ h v = modify (\s -> setTarget s h v)
  setNext _ h1 h2 = modify (\s -> setNext s h1 h2)
  setHalfedgeV _ v h = modify (\s -> setHalfedge s v h)

type instance GraphM.FaceDescriptor (SurfaceMesh v h e f) = Face

instance GraphM.FaceGraph (St v h e f) (SurfaceMesh v h e f) where
  face _ h = gets (`face` h)
  halfedgeF _ h = gets (`halfedge` h)

  nullFace _ = return nullE

instance GraphM.MutableFaceGraph (St v h e f) (SurfaceMesh v h e f) where
  addFace _ = state addFace
  removeFace _ f = modify (`removeFace` f)
  setFace _ h f = modify (\s -> setFace s h f)
  setHalfedgeF _ f h = modify (\s -> setHalfedge s f h)

-------------------------------------------------------------------------------
-- Garbage temp tests

foo :: SurfaceMesh () () () ()
foo =
  let sm = empty
      f :: St () () () () ()
      f = do
            v1 <- GraphM.addVertex sm
            v2 <- GraphM.addVertex sm
            v3 <- GraphM.addVertex sm
            Euler.addFace sm [v1, v2, v3]
            return ()
  in execState f sm

foo2 :: SurfaceMesh () () () ()
foo2 =
  let sm = empty
      f :: St () () () () ()
      f = do
            v1 <- GraphM.addVertex sm
            GraphM.addEdge sm
            return ()
  in execState f sm
