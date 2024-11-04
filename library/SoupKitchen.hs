module SoupKitchen where

import           System.Random
import           Data.Word

import           Canonical

defaultGen :: StdGen
defaultGen = mkStdGen 144

initG :: Int -> StdGen
initG = mkStdGen

range8 ::StdGen -> (Word8, StdGen)
range8 =
    uniformR ( minBound::Word8,  maxBound::Word8)

rangeC :: StdGen -> (Word8, StdGen)
rangeC =
    uniformR ( fromIntegral (fromEnum (minBound::Canonical))
             , fromIntegral (fromEnum (maxBound::Canonical)))

genOne :: StdGen -> (StdGen -> (Word8, StdGen)) -> Word8
genOne i r =
    fromIntegral $ fst $ r i

genList :: StdGen -> (StdGen -> (Word8, StdGen)) -> [Word8]
genList i r =
    genOne i r : genList (snd (r i)) r

randCGen :: StdGen -> (Word8, StdGen)
randCGen =
    randomR ( fromIntegral (fromEnum (minBound::Canonical))
            , fromIntegral (fromEnum (maxBound::Canonical)))

randCGenL :: StdGen -> [Word8]
randCGenL g =
    fst (randCGen g) : randCGenL (snd (randCGen g))

randSeedR :: (StdGen -> (Word8, StdGen)) -> Int -> [Word8]
randSeedR r s = genList (initG s) r

randSeed8 :: Int -> [Word8]
randSeed8 = randSeedR range8

randSeedC :: Int -> [Word8]
randSeedC = randSeedR rangeC
