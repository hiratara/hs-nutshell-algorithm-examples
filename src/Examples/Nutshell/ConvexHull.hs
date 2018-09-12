{-# LANGUAGE Strict #-}
module Examples.Nutshell.ConvexHull
  ( randomPoints
  , slowHull
  , greedyHull
  , Point(..)
  ) where
import Control.Monad (guard)
import qualified Data.List as List
import Data.List ((\\), sortOn, uncons)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import qualified System.Random.MWC as MWC

data Point = Point !Double !Double deriving (Eq, Ord, Show)

toTuple :: Point -> (Double, Double)
toTuple (Point x y) = (x, y)

pointNum :: Int
pointNum = 20

randomPoints :: IO (Set.Set Point)
randomPoints = MWC.withSystemRandom . MWC.asGenIO $ \gen -> do
  let ps = map (\_ -> randPt gen) [1 .. pointNum]
  Set.fromList <$> sequence ps
  where
    randPt gen = Point <$> MWC.uniform gen <*> MWC.uniform gen

slowHull :: Set.Set Point -> [Point]
slowHull ps0 =
  let ps = Set.toList ps0
      inner = do
        p0 <- ps
        let ps' = List.delete p0 ps
        p1 <- ps'
        let ps'' = List.delete p1 ps'
        p2 <- ps''
        let ps''' = List.delete p2 ps''
        p3 <- ps'''
        guard $ (p0, p1, p2) `contains` p3
        return p3
      outer = ps \\ inner
      Just (a, as') = uncons $ sortOn (\(Point x _) -> x) outer
      as = sortOn (Down . degree a) as'
  in a:as
  where
    contains :: (Point, Point, Point) -> Point -> Bool
    contains (p0, p1, p2) p3 = let b1 = prodZ p3 p0 p1 < 0.0
                                   b2 = prodZ p3 p1 p2 < 0.0
                                   b3 = prodZ p3 p2 p0 < 0.0
                               in b1 == b2 && b2 == b3

-- z of the cross product. If negative, clockwise.
prodZ :: Point -> Point -> Point -> Double
prodZ (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)

degree :: Point -> Point -> Double
degree (Point x0 y0) (Point x1 y1) =
  let len = sqrt ((x1 - x0) ^ 2 + (y1 - y0) ^ 2)
  in (y1 - y0) / len

greedyHull :: Set.Set Point -> [Point]
greedyHull pSet = go [p1, p0] ps0
  where
    (p0:ps0') = sortOn (fst . toTuple) $ Set.toList pSet
    p1:ps0 = sortOn (degree p0) ps0'
    go acum [] = acum
    go acum (p:ps) = go (p : shrink acum) ps
      where
        shrink qs@(q1:q2:qs')
          | prodZ p q1 q2 > 0 = shrink (q2:qs')
          | otherwise         = qs
