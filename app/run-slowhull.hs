module Main (main) where
import Data.Default.Class (def)
import qualified Data.Set as Set
import Examples.Nutshell.ConvexHull
  ( randomPoints
  , slowHull
  , Point(..)
  )
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import qualified Graphics.Rendering.Chart.Easy as Chart

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
