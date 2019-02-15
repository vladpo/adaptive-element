module Main where

--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo
import qualified System.Random    as Rand (mkStdGen)
import           AdaptiveElement          (fig12)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

main :: IO ()
main = print $ fig12 (Rand.mkStdGen 12)
