module SoupKitchen where

import           System.Random
import           Data.Word

import           Canonical

defaultGen :: StdGen
defaultGen = mkStdGen 144

initG :: Int -> StdGen
initG = mkStdGen

range8 ::StdGen -> (Int, StdGen)
range8 =
    uniformR (fromEnum (minBound::Word8), fromEnum (maxBound::Word8))

rangeC :: StdGen -> (Int, StdGen)
rangeC =
    uniformR (fromEnum (minBound::Canonical), fromEnum (maxBound::Canonical))

genOne :: StdGen -> (StdGen -> (Int, StdGen)) -> Word8
genOne i r =
    fromIntegral $ fst $ r i

genList :: StdGen -> (StdGen -> (Int, StdGen)) -> [Word8]
genList i r =
    genOne i r : genList (snd (r i)) r

randCGen :: StdGen -> (Int, StdGen)
randCGen =
    randomR (fromEnum (minBound::Canonical), fromEnum (maxBound::Canonical))

randCGenL :: StdGen -> [Int]
randCGenL g =
    fst (randCGen g) : randCGenL (snd (randCGen g))

randSeedR :: (StdGen -> (Int, StdGen)) -> Int -> [Word8]
randSeedR r s = genList (initG s) r

randSeed8 :: Int -> [Word8]
randSeed8 = randSeedR range8

randSeedC :: Int -> [Word8]
randSeedC = randSeedR rangeC
