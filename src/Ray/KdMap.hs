{-# LANGUAGE DeriveGeneric, CPP, FlexibleContexts #-}

module Ray.KdMap
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdMap
         -- ** /k/-d map construction
       , empty
       , emptyWithDist
       , singleton
       , singletonWithDist
       , build
       , buildWithDist
       , insertUnbalanced
       , batchInsertUnbalanced
         -- ** Query
       , nearest
       , inRadius
       , kNearest
       , inRange
       , assocs
       , keys
       , elems
       , null
       , size
         -- ** Folds
       --, foldrWithKey
         -- ** Utilities
       , defaultSqrDist
         -- ** Advanced
       , TreeNode(..)
         -- ** Internal (for testing)
       , isValid
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

--import Control.Applicative hiding (empty)

#if MIN_VERSION_base(4,8,0)
import Data.Foldable hiding (null)
#else
import Data.Foldable
import Data.Traversable
#endif

import           Prelude hiding (null)
import qualified Data.List as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Heap as Q
--import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as V

import Ray.Optics

-- $usage
--
-- The 'KdMap' is a variant of 'Data.KdTree.Static.KdTree' where each point in
-- the tree is associated with some data. When talking about 'KdMap's,
-- we'll refer to the points and their associated data as the /points/
-- and /values/ of the 'KdMap', respectively. It might help to think
-- of 'Data.KdTree.Static.KdTree' and 'KdMap' as being analogous to
-- 'Set' and 'Map'.
--
-- Suppose you wanted to perform point queries on a set of 3D points,
-- where each point is associated with a 'String'. Here's how to build
-- a 'KdMap' of the data and perform a nearest neighbor query (if this
-- doesn't make sense, start with the documentation for
-- 'Data.KdTree.Static.KdTree'):
--
-- @
-- >>> let points = [(Point3d 0.0 0.0 0.0), (Point3d 1.0 1.0 1.0)]
--
-- >>> let valueStrings = [\"First\", \"Second\"]
--
-- >>> let pointValuePairs = 'zip' points valueStrings
--
-- >>> let kdm = 'build' point3dAsList pointValuePairs
--
-- >>> 'nearest' kdm (Point3d 0.1 0.1 0.1)
-- [Point3d {x = 0.0, y = 0.0, z = 0.0}, \"First\"]
-- @

-- | A node of a /k/-d tree structure that stores a point of type @p@
-- with axis values of type @a@. Additionally, each point is
-- associated with a value of type @v@. Note: users typically will not
-- need to use this type, but we export it just in case.
data TreeNode = TreeNode { _treeLeft  :: !TreeNode
                         , _treePoint :: !(Photon, ())
                         , _axisValue :: !Double
                         , _treeRight :: !TreeNode
                         } |
                    Empty
  deriving (Generic, Show, Read)
instance NFData TreeNode where rnf = genericRnf

{-
mapTreeNode :: TreeNode -> TreeNode
mapTreeNode tn = tn
-}

-- | Converts a point of type @p@ with axis values of type
-- @a@ into a list of axis values [a].
type PointAsListFn = Photon -> [Double]

-- | Returns the squared distance between two points of type
-- @p@ with axis values of type @a@.
type SquaredDistanceFn = Photon -> Photon -> Double

-- | A /k/-d tree structure that stores points of type @p@ with axis
-- values of type @a@. Additionally, each point is associated with a
-- value of type @v@.
data KdMap = KdMap { _pointAsList :: !PointAsListFn
                   , _distSqr     :: !SquaredDistanceFn
                   , _rootNode    :: !TreeNode
                   , _size        :: !Int
                   } deriving Generic

instance NFData KdMap where rnf = genericRnf

instance Show KdMap where
  show (KdMap _ _ rootNode _) = "KdMap " ++ show rootNode

{-
instance Functor (KdMap p) where
  fmap f kdMap = kdMap { _rootNode = mapTreeNode f (_rootNode kdMap) }

foldrTreeNode :: ((p, ()) -> b -> b) -> b -> TreeNode p -> b
foldrTreeNode _ z Empty = z
foldrTreeNode f z (TreeNode left p _ right) =
  foldrTreeNode f (f p (foldrTreeNode f z right)) left

-- | Performs a foldr over each point-value pair in the 'KdMap'.
foldrWithKey :: ((p, ()) -> b -> b) -> b -> KdMap p -> b
foldrWithKey f z (KdMap _ _ r _) = foldrTreeNode f z r

instance Foldable (KdMap p) where
  foldr f = foldrWithKey (f . snd)

traverseTreeNode :: Applicative f => (() -> f ()) -> TreeNode p -> f (TreeNode p)
traverseTreeNode _ Empty = pure Empty
traverseTreeNode f (TreeNode l (p, v) axisValue r) =
  TreeNode <$>
    traverseTreeNode f l <*>
    ((,) p <$> f v) <*> -- would simply be traverse f (p, v), but
                        -- base-4.6.* doesn't have a Traversable
                        -- instance for tuples.
    pure axisValue <*>
    traverseTreeNode f r

instance Traversable (KdMap p) where
  traverse f (KdMap p d r n) =
    KdMap <$> pure p <*> pure d <*> traverseTreeNode f r <*> pure n
-}

-- | Builds an empty 'KdMap'.
empty :: PointAsListFn -> KdMap
empty p2l = emptyWithDist p2l (defaultSqrDist p2l)

-- | Builds an empty 'KdMap' using a user-specified squared distance
-- function.
emptyWithDist :: PointAsListFn
              -> SquaredDistanceFn
              -> KdMap
emptyWithDist p2l d2 = KdMap p2l d2 Empty 0

-- | Returns 'True' if the given 'KdMap' is empty.
null :: KdMap -> Bool
null kdm = _size kdm == 0

-- | Builds a 'KdMap' with a single point-value pair and a
-- user-specified squared distance function.
singletonWithDist :: PointAsListFn
                  -> SquaredDistanceFn
                  -> (Photon, ())
                  -> KdMap
singletonWithDist p2l d2 (p, v) =
  let singletonTreeNode = TreeNode Empty (p, v) (head $ p2l p) Empty
  in  KdMap p2l d2 singletonTreeNode 1

-- | Builds a 'KdMap' with a single point-value pair.
singleton :: PointAsListFn -> (Photon, ()) -> KdMap
singleton p2l (p, v) = singletonWithDist p2l (defaultSqrDist p2l) (p, v)

quickselect :: (b -> b -> Ordering) -> Int -> [b] -> b
quickselect cmp = go
  where go _ [] = error "quickselect must be called on a non-empty list."
        go k (x:xs) | k < l = go k ys
                    | k > l = go (k - l - 1) zs
                    | otherwise = x
          where (ys, zs) = L.partition ((== LT) . (`cmp` x)) xs
                l = length ys

-- | Builds a 'KdMap' from a list of pairs of points (of type p) and
-- values (of type v), using a user-specified squared distance
-- function.
--
-- Average time complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
buildWithDist :: PointAsListFn
              -> SquaredDistanceFn
              -> [(Photon, ())]
              -> KdMap
buildWithDist p2l d2 [] = emptyWithDist p2l d2
buildWithDist pointAsList distSqr dataPoints =
  let axisValsPointsPairs = zip (map (cycle . pointAsList . fst) dataPoints) dataPoints
  in  KdMap { _pointAsList = pointAsList
            , _distSqr     = distSqr
            , _rootNode    = buildTreeInternal axisValsPointsPairs
            , _size        = length dataPoints
            }
  where buildTreeInternal [] = Empty
        buildTreeInternal ps =
          let n = length ps
              (medianAxisVal : _, _) =
                quickselect (comparing (head . fst)) (n `div` 2) ps
              f ([], _) _ = error "buildKdMap.f: no empty lists allowed!"
              f (v : vt, p) (lt, maybeMedian, gt)
                | v < medianAxisVal = ((vt, p) : lt, maybeMedian, gt)
                | v > medianAxisVal = (lt, maybeMedian, (vt, p) : gt)
                | otherwise =
                    case maybeMedian of
                      Nothing -> (lt, Just p, gt)
                      Just _ -> ((vt, p) : lt, maybeMedian, gt)
              (leftPoints, maybeMedianPt, rightPoints) = L.foldr f ([], Nothing, []) ps
          in  TreeNode
              { _treeLeft  = buildTreeInternal leftPoints
              , _treePoint = fromJust maybeMedianPt
              , _axisValue = medianAxisVal
              , _treeRight = buildTreeInternal rightPoints
              }

-- | A default implementation of squared distance given two points and
-- a 'PointAsListFn'.
defaultSqrDist :: PointAsListFn -> SquaredDistanceFn
defaultSqrDist pointAsList k1 k2 =
  L.sum $ map (^ (2 :: Int)) $ zipWith (-) (pointAsList k1) (pointAsList k2)

-- | Builds a 'KdTree' from a list of pairs of points (of type p) and
-- values (of type v) using a default squared distance function
-- 'defaultSqrDist'.
--
-- Average complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
build :: PointAsListFn -> [(Photon, ())] -> KdMap
build pointAsList =
  buildWithDist pointAsList $ defaultSqrDist pointAsList

-- | Inserts a point-value pair into a 'KdMap'. This can potentially
-- cause the internal tree structure to become unbalanced. If the tree
-- becomes too unbalanced, point queries will be very inefficient. If
-- you need to perform lots of point insertions on an already existing
-- /k/-d map, check out
-- @Data.KdMap.Dynamic.@'Data.KdMap.Dynamic.KdMap'.
--
-- Average complexity: /O(log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
insertUnbalanced :: KdMap -> Photon-> () -> KdMap
insertUnbalanced kdm@(KdMap pointAsList _ rootNode n) p' v' =
  kdm { _rootNode = go rootNode (cycle $ pointAsList p'), _size = n + 1 }
  where
    go _ [] = error "insertUnbalanced.go: no empty lists allowed!"
    go Empty (axisValue' : _) = TreeNode Empty (p', v') axisValue' Empty
    go t@(TreeNode left _ nodeAxisValue right) (axisValue' : nextAxisValues)
      | axisValue' <= nodeAxisValue = t { _treeLeft = go left nextAxisValues }
      | otherwise = t { _treeRight = go right nextAxisValues }

-- | Inserts a list of point-value pairs into a 'KdMap'. This can
-- potentially cause the internal tree structure to become unbalanced,
-- which leads to inefficient point queries.
--
-- Average complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
batchInsertUnbalanced :: KdMap -> [(Photon, ())] -> KdMap
batchInsertUnbalanced = foldl' $ \kdm (p, v) -> insertUnbalanced kdm p v

assocsInternal :: TreeNode -> [(Photon, ())]
assocsInternal t = go t []
  where go Empty = id
        go (TreeNode l p _ r) = go l . (p :) . go r

-- | Returns a list of all the point-value pairs in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
assocs :: KdMap -> [(Photon, ())]
assocs (KdMap _ _ t _) = assocsInternal t

-- | Returns all points in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
keys :: KdMap -> [Photon]
keys = map fst . assocs

-- | Returns all values in the 'KdMap'.
--
-- Time complexity: /O(n)/ for /n/ data points.
elems :: KdMap -> [()]
elems = map snd . assocs

-- | Given a 'KdMap' and a query point, returns the point-value pair
-- in the 'KdMap' with the point nearest to the query.
--
-- Average time complexity: /O(log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
--
-- Throws error if called on an empty 'KdMap'.
nearest :: KdMap -> Photon -> (Photon, ())
nearest (KdMap _ _ Empty _) _ =
  error "Attempted to call nearest on an empty KdMap."
nearest (KdMap pointAsList distSqr t@(TreeNode _ root _ _) _) query =
  -- This is an ugly way to kickstart the function but it's faster
  -- than using a Maybe.
  fst $ go (root, distSqr (fst root) query) (cycle $ pointAsList query) t
  where
    go _ [] _ = error "nearest.go: no empty lists allowed!"
    go bestSoFar _ Empty = bestSoFar
    go bestSoFar
       (queryAxisValue : qvs)
       (TreeNode left (nodeK, nodeV) nodeAxisVal right) =
      let better x1@(_, dist1) x2@(_, dist2) = if dist1 < dist2
                                               then x1
                                               else x2
          currDist       = distSqr query nodeK
          bestAfterNode = better ((nodeK, nodeV), currDist) bestSoFar
          nearestInTree onsideSubtree offsideSubtree =
            let bestAfterOnside = go bestAfterNode qvs onsideSubtree
                checkOffsideSubtree =
                  (queryAxisValue - nodeAxisVal)^(2 :: Int) < snd bestAfterOnside
            in  if checkOffsideSubtree
                then go bestAfterOnside qvs offsideSubtree
                else bestAfterOnside
      in  if queryAxisValue <= nodeAxisVal
          then nearestInTree left right
          else nearestInTree right left

-- | Given a 'KdMap', a query point, and a radius, returns all
-- point-value pairs in the 'KdMap' with points within the given
-- radius of the query point.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for /n/ data points and a radius
-- that spans all points in the structure.
inRadius :: KdMap
         -> Double -- ^ radius
         -> Photon -- ^ query point
         -> V.Vector (Photon, ()) -- ^ list of point-value pairs with
                     -- points within given radius of query
inRadius (KdMap pointAsList distSqr t _) radius query =
  go (cycle $ pointAsList query) t V.empty
  where
    go [] _ _ = error "inRadius.go: no empty lists allowed!"
    go _ Empty acc = acc
    go (queryAxisValue : qvs) (TreeNode left (k, v) nodeAxisVal right) acc =
      let onTheLeft = queryAxisValue <= nodeAxisVal
          accAfterOnside = if   onTheLeft
                           then go qvs left acc
                           else go qvs right acc
          accAfterOffside = if   abs (queryAxisValue - nodeAxisVal) < radius
                            then if   onTheLeft
                                 then go qvs right accAfterOnside
                                 else go qvs left accAfterOnside
                            else accAfterOnside
          accAfterCurrent = if distSqr k query <= radius * radius
                            then V.cons (k, v) accAfterOffside
                            else accAfterOffside
      in  accAfterCurrent
{-
radianceInRadius ::
     KdMap
  -> Double -- ^ radius
  -> Photon -- ^ query point
  -> V.Vector (Photon, ()) -- ^ list of point-value pairs with
                     -- points within given radius of query
radianceInRadius (KdMap pointAsList distSqr t _) radius query =
  go (cycle $ pointAsList query) t V.empty
  where
    go [] _ _ = error "inRadius.go: no empty lists allowed!"
    go _ Empty acc = acc
    go (queryAxisValue : qvs) (TreeNode left (k, v) nodeAxisVal right) acc =
      let onTheLeft = queryAxisValue <= nodeAxisVal
          accAfterOnside = if   onTheLeft
                           then go qvs left acc
                           else go qvs right acc
          accAfterOffside = if   abs (queryAxisValue - nodeAxisVal) < radius
                            then if   onTheLeft
                                 then go qvs right accAfterOnside
                                 else go qvs left accAfterOnside
                            else accAfterOnside
          accAfterCurrent = if distSqr k query <= radius * radius
                            then V.cons (k, v) accAfterOffside
                            else accAfterOffside
      in  accAfterCurrent
-}

-- | Given a 'KdMap', a query point, and a number @k@, returns the @k@
-- point-value pairs with the nearest points to the query.
--
-- Neighbors are returned in order of increasing distance from query
-- point.
--
-- Average time complexity: /log(k) * log(n)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
--
-- Worst case time complexity: /n * log(k)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
kNearest :: KdMap -> Int -> Photon-> [(Photon, ())]
kNearest (KdMap pointAsList distSqr t _) numNeighbors query =
  reverse $ map snd $ Q.toAscList $ go (cycle $ pointAsList query)
    (Q.empty :: Q.MaxPrioHeap a (p,v)) t
  where
    -- go :: [a] -> Q.MaxPrioHeap a (p, v) -> TreeNode a p v -> Q.MaxPrioHeap a (p, v)
    go [] _ _ = error "kNearest.go: no empty lists allowed!"
    go _ q Empty = q
    go (queryAxisValue : qvs) q (TreeNode left (k, v) nodeAxisVal right) =
      let insertBounded queue dist x
            | Q.size queue < numNeighbors = Q.insert (dist, x) queue
            | otherwise = let ((farthestDist, _), rest) = fromJust $ Q.view queue
                          in  if dist < farthestDist
                              then Q.insert (dist, x) rest
                              else queue
          q' = insertBounded q (distSqr k query) (k, v)
          kNear queue onsideSubtree offsideSubtree =
            let queue' = go qvs queue onsideSubtree
                checkOffsideTree =
                  Q.size queue' < numNeighbors ||
                  (queryAxisValue - nodeAxisVal)^(2 :: Int) <
                    (fst . fst) (fromJust $ Q.view queue')
            in  if checkOffsideTree
                then go qvs queue' offsideSubtree
                else queue'
      in  if queryAxisValue <= nodeAxisVal
          then kNear q' left right
          else kNear q' right left

-- | Finds all point-value pairs in a 'KdMap' with points within a
-- given range, where the range is specified as a set of lower and
-- upper bounds.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for n data points and a range
-- that spans all the points.
--
-- TODO: Maybe use known bounds on entire tree structure to be able to
-- automatically count whole portions of tree as being within given
-- range.
inRange :: KdMap
        -> Photon -- ^ lower bounds of range
        -> Photon -- ^ upper bounds of range
        -> [(Photon, ())] -- ^ point-value pairs within given
                              -- range
inRange (KdMap pointAsList _ t _) lowers uppers =
  go (cycle (pointAsList lowers) `zip` cycle (pointAsList uppers)) t []
  where
    go [] _ _ = error "inRange.go: no empty lists allowed!"
    go _ Empty acc = acc
    go ((lower, upper) : nextBounds) (TreeNode left p nodeAxisVal right) acc =
      let accAfterLeft = if lower <= nodeAxisVal
                         then go nextBounds left acc
                         else acc
          accAfterRight = if upper > nodeAxisVal
                          then go nextBounds right accAfterLeft
                          else accAfterLeft
          valInRange l x u = l <= x && x <= u
          -- maybe "cache" lowers and uppers as lists sooner as hint
          -- to ghc. Also, maybe only need to check previously
          -- unchecked axes?
          currentInRange =
            L.and $ zipWith3 valInRange
              (pointAsList lowers) (pointAsList $ fst p) (pointAsList uppers)
          accAfterCurrent = if currentInRange
                            then p : accAfterRight
                            else accAfterRight
      in  accAfterCurrent

-- | Returns the number of point-value pairs in the 'KdMap'.
--
-- Time complexity: /O(1)/
size :: KdMap -> Int
size (KdMap _ _ _ n) = n

isTreeNodeValid :: PointAsListFn -> Int -> TreeNode -> Bool
isTreeNodeValid _ _ Empty = True
isTreeNodeValid pointAsList axis (TreeNode l (k, _) nodeAxisVal r) =
  let childrenAxisValues = map ((!! axis) . pointAsList . fst) . assocsInternal
      leftSubtreeLess = L.all (<= nodeAxisVal) $ childrenAxisValues l
      rightSubtreeGreater = L.all (> nodeAxisVal) $ childrenAxisValues r
      nextAxis = (axis + 1) `mod` length (pointAsList k)
  in  leftSubtreeLess && rightSubtreeGreater &&
      isTreeNodeValid pointAsList nextAxis l && isTreeNodeValid pointAsList nextAxis r

-- | Returns 'True' if tree structure adheres to k-d tree
-- properties. For internal testing use.
isValid :: KdMap -> Bool
isValid (KdMap pointAsList _ r _) = isTreeNodeValid pointAsList 0 r
