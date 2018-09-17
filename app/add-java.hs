{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}
import Criterion.Main (defaultMain, bench, nfIO)
import Data.Int
import Data.Vector.Storable (fromList, toList, thaw, freeze)
import Data.Vector.Storable.Mutable (IOVector)
import Language.Java (withJVM, reflect, reify, JType(..), J)
import Language.Java.Inline

javaAdd :: IOVector Int32 -> IOVector Int32 -> IO (IOVector Int32)
javaAdd n1 n2 = do
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
  n1 <- thaw $ fromList [0, 1..9]
  n2 <- thaw $ fromList $ 0 : [9, 8..1]
  withJVM [] $ do
    defaultMain
      [ bench "javaAdd" $ nfIO $ javaAdd n1 n2
      ]