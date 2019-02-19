{-# LANGUAGE DeriveGeneric #-}

module Main where

--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo
import qualified System.Random    as Rand (mkStdGen)
import           AdaptiveElement          (fig12, d)
import Control.Monad.State (runState)
import Numeric.Probability.Random ( pick, decons)
import  Numeric.Probability.Distribution ( just
                                         , (??)
                                         )
import Text.Printf

main :: IO ()
main = writeFile "/home/vpo/workspace/adaptive-element/out.csv" $ foldl (\s t -> s ++ show (snd t) ++ "," ++ printf "%.4f" (fst t) ++ "\r\n") "ITI,Lambda\r\n" (fig12 $ Rand.mkStdGen 12)
--main = print $ runState (decons $ pick d) (Rand.mkStdGen 13)