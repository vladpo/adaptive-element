{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UnicodeSyntax    #-}

module AdaptiveElement
    ( fig12
    ) where

import            Control.Lens                      hiding ((??))
import            Control.Monad.State               (runState)
import            Debug.Trace                       as T (trace)
import qualified  Numeric.Probability.Shape         as NPS ( normalCurve)
import qualified  Numeric.Probability.Distribution  as NPD ( shape
                                                           )
import qualified  Numeric.Probability.Random        as NPR ( pick
                                                           , decons
                                                           )
import qualified System.Random                      as Rand (StdGen)
import           Data.List                                 (genericLength)

data Constants = C { alfa :: Double
                   , beta :: Double
                   , gamma :: Double
                   , lambda :: Double
                   , mean :: Double
                   , deviation :: Double
                   }
type Weight = Double
type Weights = [Weight]
type Input = Double
type Inputs = [Input]
type InTrace = Double
type InTraces = [InTrace]
type Output = Double
type OutTrace = Double

tmul :: Num a => (a, a) -> a
tmul (x, y) = x*y

inputTrace :: Constants -> Inputs -> InTraces -> InTraces
inputTrace env xs xts = itrace (alfa env) <$> zip xs xts
  where
    itrace :: Double -> (Input, InTrace) -> InTrace
    itrace alfa t = alfa * snd t + fst t

initWeight :: Weights
initWeight = replicate 4 0.01

weights :: Constants -> Weights -> Output -> OutTrace -> InTraces -> Weights
weights env ws y y' xts = loop [] $ zip ws xts
  where
    odiff = y - y'
    g = gamma env
    loop :: Weights -> [(Weight, InTrace)] -> Weights
    loop nws ((w, _):[]) = w:nws
    loop nws (t:wxs) = weight t : loop nws wxs
      where
        weight :: (Weight, InTrace) -> Weight
        weight t = fst t + g*odiff* snd t

output :: Weights -> Inputs -> Output
output ws xs = min 1.0 (max 0.0 $ foldl (\sum -> (sum+) . tmul) 0.0 $ zip ws xs)

outputTrace :: Constants -> Output -> OutTrace -> OutTrace
outputTrace env y y' = b*y' + (1 - b)*y
  where b = beta env

randomNoise :: Constants -> Rand.StdGen -> [Double] -> (Double, Rand.StdGen)
randomNoise env g xs = flip runState g $ NPR.decons (NPR.pick $ NPD.shape (NPS.normalCurve m d) xs)
  where (m, d) = (mean env, deviation env)

noiseValues :: [Double]
noiseValues = map (/1000.0) [negate 100..100]

stimulus :: Int -> Int -> Int -> [Double]
stimulus predelay duration postdelay = concat $ replicate 20 (replicate predelay 0.0 ++ replicate duration 1.0 ++ replicate postdelay 0.0 ++ replicate 50 0.0)

addNoise :: Constants -> Rand.StdGen -> Double -> [Double] -> (Double, Rand.StdGen)
addNoise env g v nvs = let (n, ng) = randomNoise env g nvs in (max 0.0 $ v + n, ng)

conditionedStimulus :: Int -> [Double]
conditionedStimulus iti = stimulus 1 3 (iti+30)

unconditionedStimulus :: Int -> [Double]
unconditionedStimulus iti = stimulus (iti+1) 30 3

learn :: Constants -> Weights -> [[Double]] -> InTraces -> Output -> OutTrace -> Double
learn _ ws [] _ _ _ = sum (init ws) / 4.0
learn env ws (xs:xss) xs' y y' = learn env ws' xss nxs' ny y
  where
    ny = output ws xs
    nxs' = inputTrace env xs xs'
    ws' = debug $ weights env ws y y' xs'

inputs :: Constants -> Int -> Rand.StdGen -> [Double] -> ([[Double]], Rand.StdGen)
inputs env iti g nvs = foldr f ([], g) $ zip cs ucs
  where
    cs = conditionedStimulus iti
    ucs = unconditionedStimulus iti
    f :: (Double, Double) -> ([[Double]], Rand.StdGen) -> ([[Double]], Rand.StdGen)
    f (x, x') (xss, g) = let (xs, ng) = foldr (\x t -> let (ny, nng) = addNoise env (snd t) x nvs in (ny:fst t, nng)) ([], g) $ replicate 4 x ++ [x'] in (xs:xss, ng)

fig12 :: Rand.StdGen -> [(Double, Int)]
fig12 g = fst $ foldr (\iti t ->  let (xss, ng) = inputs env iti (snd t) nvs in ((learn env (initWeight++[lambda env]) xss (replicate 5 0.0) 0.0 0.0, iti) : fst t, ng)) ([], g) [3]
  where
    env = C { alfa = 0.9, beta = 0.0, gamma = 0.2, lambda = 0.6, mean = 0.005, deviation = 0.03}
    nvs = noiseValues

debug :: Show a => a -> a
debug a | T.trace (show a) False = undefined
debug a = a