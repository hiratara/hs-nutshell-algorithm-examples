{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}
import Criterion.Main (defaultMain, bench, whnfIO)
import Data.Int
import Data.Vector.Storable (fromList, toList, thaw, freeze)
import Data.Vector.Storable.Mutable (IOVector)
import Language.Java (withJVM, reflect, reify, JType(..), J)
import Language.Java.Inline
import qualified System.Random.MWC as MWC

randomNum :: Int -> IO (IOVector Int32)
randomNum n = MWC.withSystemRandom . MWC.asGenIO $ \gen -> do
  let ps = map (\_ -> MWC.uniformR (0, 9) gen) [1 .. n] :: [IO Int32]
  fromList <$> sequence ps >>= thaw

javaAdd :: J ('Array ('Prim "int")) -> J ('Array ('Prim "int")) -> IO (J ('Array ('Prim "int")))
javaAdd jn1 jn2 = do
  jr <- [java| {
      int[] sum = new int[$jn1.length + 1];
      int position = $jn1.length - 1;
      int carry = 0;
      while (position >= 0) {
          int total = $jn1[position] + $jn2[position] + carry;
          sum[position + 1] = total % 10;
          if (total > 9) { carry = 1; } else { carry = 0; }
          position--;
      }
      sum[0] = carry;
      return sum;
  } |]
  return jr

javaAdd' :: IOVector Int32 -> IOVector Int32 -> IO (IOVector Int32)
javaAdd' n1 n2 = do
    jn1 <- reflect n1
    jn2 <- reflect n2
    jr <- [java| {
        int[] sum = new int[$jn1.length + 1];
        int position = $jn1.length - 1;
        int carry = 0;
        while (position >= 0) {
            int total = $jn1[position] + $jn2[position] + carry;
            sum[position + 1] = total % 10;
            if (total > 9) { carry = 1; } else { carry = 0; }
                position--;
            }
            sum[0] = carry;
            return sum;
        } |]
    reify jr

main :: IO ()
main = do
  n1 <- randomNum 256
  n2 <- randomNum 256
  withJVM [] $ do
    jn1 <- reflect (n1 :: IOVector Int32)
    jn2 <- reflect (n2 :: IOVector Int32)
    defaultMain
      [ bench "javaAdd" $ whnfIO $ javaAdd jn1 jn2
      , bench "javaAdd-dash" $ whnfIO $ javaAdd' n1 n2
      ]
