module Main (main) where
import Criterion.Main (defaultMain, bench, nf)
import qualified Data.Set as Set
import Examples.Nutshell.ConvexHull
  ( randomPoints
  , slowHull
  , greedyHull
  , Point(..)
  )

impl :: Maybe String -> (Set.Set Point -> [Point])
impl (Just "greedy") = greedyHull
impl (Just "slow") = slowHull
impl _ = slowHull

main :: IO ()
main = do
  ps <- randomPoints
  putStrLn "points generated"
  print ps

  defaultMain
    [ bench "slow" $ nf slowHull ps
    , bench "greedy" $ nf greedyHull ps
    ]
