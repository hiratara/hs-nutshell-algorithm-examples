module Main where
import Control.Monad (guard)
import Data.Default.Class (def)
import qualified Data.List as List
import Data.List ((\\), sortOn, uncons)
import qualified Data.Set as Set
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified System.Random.MWC as MWC

data Point = Point !Double !Double deriving (Eq, Ord, Show)

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
      as = sortOn (degree a) as'
  in a:as
  where
    contains :: (Point, Point, Point) -> Point -> Bool
    contains (p0, p1, p2) p3 = let b1 = prodZ p3 p0 p1 < 0.0
                                   b2 = prodZ p3 p1 p2 < 0.0
                                   b3 = prodZ p3 p2 p0 < 0.0
                               in b1 == b2 && b2 == b3
    prodZ :: Point -> Point -> Point -> Double
    prodZ (Point x1 y1) (Point x2 y2) (Point x3 y3) =
      (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
    degree :: Point -> Point -> Double
    degree (Point x0 y0) (Point x1 y1) =
      y1 - y0 / sqrt ((x1 - x0) ^ 2 + (y1 - y0) ^ 2)

main :: IO ()
main = do
  ps <- randomPoints
  putStrLn "points generated"
  print ps
  putStrLn "convex hull"
  let hull = slowHull ps
  print hull

  let pslist = map (\(Point x y) -> (x, y)) . Set.toList $ ps
      hulllist = map (\(Point x y) -> (x, y)) hull
      chart = Chart.toRenderable . Chart.execEC
            $ do Chart.plot $ Chart.points "" pslist
                 Chart.plot $ Chart.line "" [last hulllist : hulllist]
  Diagrams.renderableToFile def "slowhull.svg" chart

  return ()
