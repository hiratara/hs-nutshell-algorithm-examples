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
import System.Environment (getArgs)

impl :: Maybe String -> (Set.Set Point -> [Point])
impl _ = slowHull

main :: IO ()
main = do
  argv <- getArgs
  ps <- randomPoints
  putStrLn "points generated"
  print ps
  putStrLn "convex hull"
  let convexHull = impl (case argv of [] -> Nothing; _ -> Just $ head argv)
      hull = convexHull ps
  print hull

  let pslist = map (\(Point x y) -> (x, y)) . Set.toList $ ps
      hulllist = map (\(Point x y) -> (x, y)) hull
      chart = Chart.toRenderable . Chart.execEC
            $ do Chart.plot $ Chart.points "" pslist
                 Chart.plot $ Chart.line "" [last hulllist : hulllist]
  Diagrams.renderableToFile def "slowhull.svg" chart

  return ()
