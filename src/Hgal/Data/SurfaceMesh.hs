{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hgal.Data.SurfaceMesh where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.Either
import Data.Foldable (length)
import Data.Maybe
import Data.Sequence hiding (empty, length)
import qualified Data.Sequence as Seq (empty)
import Data.Tuple

import qualified Hgal.Graph.Class as Graph
import qualified Hgal.Graph.ClassM as GraphM
import Hgal.Graph.Loops

import Debug.Trace
import qualified Hgal.Graph.EulerOperations as Euler
import qualified Hgal.Graph.EulerOperationsM as EulerM

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

data family Connectivity a :: *

class Property s k p | k -> p, p -> k, k -> s where
  type Container k :: * -> *

  -- pGet :: s -> k -> Maybe p
  -- pAdjust :: (p -> p) -> k -> s -> s
  property :: k -> Lens' s (Maybe p)
  properties :: s -> (Container k) p

class Element a where
  data Size a :: *

  name :: a -> String

  numElements :: SurfaceMesh v d -> Size a
  new :: SurfaceMesh v d -> a

  nullE :: a
  default nullE :: Bounded a => a
  nullE = maxBound

  conn :: a -> Lens' (SurfaceMesh v d) (Connectivity a)

  propertyOf :: Property s a p => a -> Lens' (SurfaceMesh v s) (Maybe p)

  hasValidIndex :: SurfaceMesh v d -> a -> Bool
  hasValidConnectivity :: SurfaceMesh v d -> a -> Either String Bool

  isValid :: SurfaceMesh v d -> a -> Either String Bool
  isValid sm v
    | hasValidIndex sm v = hasValidConnectivity sm v
    | otherwise = Right False

  isBorder :: SurfaceMesh v d -> a -> Bool

  isIsolated :: SurfaceMesh v d -> a -> Bool
  isIsolated sm = (== nullE) . halfedge sm

  isRemoved :: SurfaceMesh v d -> a -> Bool
  isRemoved _ _ = False

  degree :: SurfaceMesh v d -> a -> Int

  vertex :: SurfaceMesh v d -> a -> Vertex
  vertex sm = vertex sm . halfedge sm

  halfedge :: SurfaceMesh v d -> a -> Halfedge

  setHalfedge :: SurfaceMesh v d -> a -> Halfedge -> SurfaceMesh v d
  setHalfedge sm _ _ = sm

  edge :: SurfaceMesh v d -> a -> Edge
  edge sm = (\(Halfedge i) -> Edge i) . halfedge sm

  face :: SurfaceMesh v d -> a -> Face
  face sm = face sm . halfedge sm

  next :: SurfaceMesh v d -> a -> a
  prev :: SurfaceMesh v d -> a -> a

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

data SurfaceMesh v d = SurfaceMesh
  { _vconn :: Seq (Connectivity Vertex)
  , _hconn :: Seq (Connectivity Halfedge)
  , _fconn :: Seq (Connectivity Face)
  , _vpoint :: Seq v
  , _props :: d
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
  propertyOf e = props.property e

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

  degree sm v
    | h == nullE = 0
    | otherwise = length $ verticesAroundTarget sm h
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
  propertyOf e = props.property e

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

  isBorder sm = (== nullE) . face sm

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

  propertyOf e = props.property (div e 2)

  hasValidIndex sm (Edge i) = i < n
    where (EdgeSize n) = numElements sm

  isValid sm e
    | hasValidIndex sm e = collectErrors [isValid sm h, isValid sm (opposite h)]
    | otherwise = Right False
    where h = halfedge sm e

  isBorder sm e = isBorder sm h || isBorder sm (opposite h)
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
  propertyOf e = props.property e

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

  degree sm v
    | h == nullE = 0
    | otherwise = length $ verticesAroundFace sm h
    where h = halfedge sm v

  halfedge sm f = view (conn f.fH) sm
  setHalfedge sm f h = set (conn f.fH) h sm
  face _ f = f

  newtype Size Face = FaceSize Int
    deriving (Bounded, Enum, Eq, Ord, Show)
    deriving newtype (Bits, Integral, Num, Real)


empty :: d -> SurfaceMesh v d
empty = SurfaceMesh
   Seq.empty Seq.empty Seq.empty
   Seq.empty

-------------------------------------------------------------------------------
-- Elements

vertices :: SurfaceMesh v d -> [Vertex]
vertices sm = Prelude.filter (not . isRemoved sm) $
              Vertex <$> [0..fromIntegral (numElements sm :: Size Vertex) - 1]

halfedges :: SurfaceMesh v d -> [Halfedge]
halfedges sm = Prelude.filter (not . isRemoved sm) $
               Halfedge <$> [0..fromIntegral (numElements sm :: Size Halfedge) - 1]

edges :: SurfaceMesh v d -> [Edge]
edges sm = Prelude.filter (not . isRemoved sm) $
           Edge . (* 2) <$> [0..fromIntegral (numElements sm :: Size Edge) - 1]

faces :: SurfaceMesh v d -> [Face]
faces sm = Prelude.filter (not . isRemoved sm) $
           Face <$> [0..fromIntegral (numElements sm :: Size Face) - 1]

-------------------------------------------------------------------------------
-- Adding elements

addVertex :: SurfaceMesh v d -> (Vertex, SurfaceMesh v d)
addVertex sm =
  let sm' = (vconn._Wrapped') %~ (snoc ?? VertexConnectivity nullE) $ sm
  in (new sm, sm')

addEdge :: SurfaceMesh v d -> (Edge, SurfaceMesh v d)
addEdge sm =
  let addh = (snoc ?? HalfedgeConnectivity nullE nullE nullE nullE)
      sm' = (hconn._Wrapped') %~ (addh . addh) $ sm
  in (new sm, sm')

addFace :: SurfaceMesh v d -> (Face, SurfaceMesh v d)
addFace sm =
  let sm' = (fconn._Wrapped') %~ (snoc ?? FaceConnectivity nullE) $ sm
  in (new sm, sm')

newVertex :: SurfaceMesh v d -> v -> (SurfaceMesh v d, Vertex)
newVertex sm v =
  --that's not exactly right
  let sm' = vpoint %~ (snoc ?? v) $ sm
  in swap $ addVertex sm'

newFace :: Foldable t => SurfaceMesh v d -> t Vertex -> (SurfaceMesh v d, Face)
newFace sm vs = swap $ Euler.addFace sm vs

-------------------------------------------------------------------------------
-- Removing elements

removeVertex :: SurfaceMesh v d -> Vertex -> SurfaceMesh v d
removeVertex sm v = sm

removeEdge :: SurfaceMesh v d -> Edge -> SurfaceMesh v d
removeEdge sm v = sm

removeFace :: SurfaceMesh v d -> Face -> SurfaceMesh v d
removeFace sm v = sm

-------------------------------------------------------------------------------
-- Connectivity

target :: SurfaceMesh v d -> Halfedge -> Vertex
target sm h = view (conn h.hV) sm

setTarget :: SurfaceMesh v d -> Halfedge -> Vertex -> SurfaceMesh v d
setTarget sm h v = set (conn h.hV) v sm

setFace :: SurfaceMesh v d -> Halfedge -> Face -> SurfaceMesh v d
setFace sm h f = set (conn h.hF) f sm

setNextOnly :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setNextOnly sm h nh = set (conn h.hN) nh sm

setPrevOnly :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setPrevOnly sm h ph = set (conn h.hP) ph sm

setNext :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setNext sm h nh = setPrevOnly (setNextOnly sm h nh) nh h

opposite :: Halfedge -> Halfedge
opposite h = xor h 1

source :: SurfaceMesh v d -> Halfedge -> Vertex
source sm = target sm . opposite


halfedgeVV :: SurfaceMesh v d -> Vertex -> Vertex -> Maybe Halfedge
halfedgeVV sm sour tar =
  assert (hasValidIndex sm sour && hasValidIndex sm tar) result
  where
    h = halfedge sm tar
    worker hx =
      let n = opposite (next sm hx)
      in
        if source sm hx == sour then Just hx
          else if n /= h then worker n
             else Nothing
    result = if h == nullE then Nothing
      else worker h

-------------------------------------------------------------------------------
-- Properties

vertexProperties :: Property d Vertex p => SurfaceMesh v d -> Container Vertex p
vertexProperties sm = properties (_props sm)

halfedgeProperties :: Property d Halfedge p => SurfaceMesh v d -> Container Halfedge p
halfedgeProperties sm = properties (_props sm)

edgeProperties :: Property d Edge p => SurfaceMesh v d -> Container Edge p
edgeProperties sm = properties (_props sm)

faceProperties :: Property d Face p => SurfaceMesh v d -> Container Face p
faceProperties sm = properties (_props sm)

-------------------------------------------------------------------------------
-- Full integrity check

checkIntegrity :: SurfaceMesh v d -> Either String Bool
checkIntegrity sm = collectErrors [hsI, vsI]
  where
    hsI = second and (mapM (halfedgeIntegrity sm) $ halfedges sm)
    vsI = second and (mapM (vertexIntegrity sm) $ vertices sm)

halfedgeIntegrity :: SurfaceMesh v d -> Halfedge -> Either String Bool
halfedgeIntegrity sm h = halfedgeI >> oppositeI >> previousI >> nextI >> vertexI >> nextOppositeI
  where
    halfedgeI =
      first (\s -> "Integrity of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid sm (next sm h), isValid sm (opposite h)]
    oppositeI
      | opposite h /= h && opposite (opposite h) == h = Right True
      | otherwise = Left $ "Integrity of opposite halfedge of " ++ show h ++ " corrupted.\n"
    previousI
      | next sm (prev sm h) == h = Right True
      | otherwise = Left $ "Integrity of previous halfedge of " ++ show h ++ " corrupted.\n"
    nextI
      | prev sm (next sm h) == h = Right True
      | otherwise = Left $ "Integrity of next halfedge of " ++ show h ++ " corrupted.\n"
    vertexI =
      first (\s -> "Integrity of vertex of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid sm (target sm h)]
    nextOppositeI
      | target sm (opposite (next sm h)) == target sm h = Right True
      | otherwise = Left $ "Halfedge vertex of next opposite is not the same for " ++ show h ++ ".\n"

vertexIntegrity :: SurfaceMesh v d -> Vertex -> Either String Bool
vertexIntegrity sm v
  | fromRight False (isValid sm h) && target sm (halfedge sm v) == v = Right True
  | otherwise = Left $ "Halfedge of vertex " ++ show v ++ " is not an incoming halfedge.\n"
  where h = halfedge sm v

-------------------------------------------------------------------------------
-- string & error helpers

showConnectivity :: (Show a, Foldable t) => String -> t a -> String
showConnectivity t s =
  t ++ " :: " ++ ifoldMapOf folded
  (\i a -> "\n(" ++ show i ++ ", " ++ show a ++ ")") s

conError :: (Element a, Element b, Show a, Show b)
         => SurfaceMesh v d -> a -> String -> b -> String
conError sm els s elt =
  name els ++ " " ++ show els ++ " conneciivity error. " ++
  s ++ " " ++ name elt ++ " " ++ show elt ++ " is " ++
  if hasValidIndex sm elt then "removed." else "invalid."

collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left

putIntegrity :: SurfaceMesh v d -> IO ()
putIntegrity = putStr . fromLeft "All correct.\n" . checkIntegrity

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

instance Show (SurfaceMesh v d) where
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

type instance Graph.VertexDescriptor (SurfaceMesh v d) = Vertex
type instance Graph.HalfedgeDescriptor (SurfaceMesh v d) = Halfedge
type instance Graph.EdgeDescriptor (SurfaceMesh v d) = Edge

instance Graph.HalfedgeGraph (SurfaceMesh v d) where
  edge = edge
  halfedgeV = halfedge
  halfedgeE = halfedge
  halfedgeVV = halfedgeVV
  opposite _ = opposite
  source = source
  target = target
  next = next
  prev = prev

  isBorderH = isBorder
  isBorderV = isBorder
  nullHalfedge _ = nullE

instance Graph.MutableHalfedgeGraph (SurfaceMesh v d) where
  addVertex = addVertex
  removeVertex = removeVertex
  addEdge = addEdge
  removeEdge = removeEdge
  setTarget = setTarget
  setNext = setNext
  setHalfedgeV = setHalfedge

type instance Graph.FaceDescriptor (SurfaceMesh v d) = Face

instance Graph.FaceGraph (SurfaceMesh v d) where
  face = face
  halfedgeF = halfedge

  nullFace _ = nullE

instance Graph.MutableFaceGraph (SurfaceMesh v d) where
  addFace = addFace
  removeFace = removeFace
  setFace = setFace
  setHalfedgeF = setHalfedge

type instance GraphM.VertexDescriptor (SurfaceMesh v d) = Vertex
type instance GraphM.HalfedgeDescriptor (SurfaceMesh v d) = Halfedge
type instance GraphM.EdgeDescriptor (SurfaceMesh v d) = Edge

type St v d = State (SurfaceMesh v d)

-- instance GraphM.HalfedgeGraphS (St v h e f) (SurfaceMesh v d)

instance GraphM.HalfedgeGraph (St v d) (SurfaceMesh v d) where
  edge _ h = gets (`edge` h)
  halfedgeV _ h = gets (`halfedge` h)
  halfedgeE _ h = gets (`halfedge` h)
  halfedgeVV _ h v = gets (\s -> halfedgeVV s h v)
  opposite _ h = return (opposite h)
  source _ h = gets (`source` h)
  target _ h = gets (`target` h)
  next _ h = gets (`next` h)
  prev _ h = gets (`prev` h)

  isBorderH _ h = gets (`isBorder` h)
  isBorderV _ h = gets (`isBorder` h)
  nullHalfedge _ = return nullE

  showM _ = gets show

instance GraphM.MutableHalfedgeGraph (St v d) (SurfaceMesh v d) where
  addVertex _ = state addVertex
  removeVertex _ v = modify (`removeVertex` v)
  addEdge _ = state addEdge
  removeEdge _ v = modify (`removeEdge` v)
  setTarget _ h v = modify (\s -> setTarget s h v)
  setNext _ h1 h2 = modify (\s -> setNext s h1 h2)
  setHalfedgeV _ v h = modify (\s -> setHalfedge s v h)

type instance GraphM.FaceDescriptor (SurfaceMesh v d) = Face

instance GraphM.FaceGraph (St v d) (SurfaceMesh v d) where
  face _ h = gets (`face` h)
  halfedgeF _ h = gets (`halfedge` h)

  nullFace _ = return nullE

instance GraphM.MutableFaceGraph (St v d) (SurfaceMesh v d) where
  addFace _ = state addFace
  removeFace _ f = modify (`removeFace` f)
  setFace _ h f = modify (\s -> setFace s h f)
  setHalfedgeF _ f h = modify (\s -> setHalfedge s f h)

-------------------------------------------------------------------------------
-- Garbage temp tests

foo :: SurfaceMesh () ()
foo =
  let sm = empty ()
      f :: St () () ()
      f = do
            v1 <- GraphM.addVertex sm
            v2 <- GraphM.addVertex sm
            v3 <- GraphM.addVertex sm
            EulerM.addFace sm [v1, v2, v3]
            return ()
  in execState f sm

newtype MyProps2 = MyProps2 (IntMap Int, IntMap (Map String String))

instance Property MyProps2 Vertex Int where
  type Container Vertex = IntMap
  property (Vertex i) g (MyProps2 m) = MyProps2 <$> (_1.at i) g m
  properties (MyProps2 m) = fst m

instance Property MyProps2 Edge (Map String String) where
  type Container Edge = IntMap
  property (Edge i) g (MyProps2 m) = MyProps2 <$> (_2.at i) g m
  properties (MyProps2 m) = snd m

foo2 :: SurfaceMesh () MyProps2
foo2 =
  let sm = empty (MyProps2 (IntMap.empty, IntMap.empty))
      f = do
            v1 <- GraphM.addVertex sm
            GraphM.addEdge sm
            propertyOf (Vertex 0) ?= 7
            propertyOf (Edge 0) ?= Map.fromList [("foo", "bar")]
            (propertyOf (Edge 0)._Just.at "foo") ?= "alice"
  in execState f sm
