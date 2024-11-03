module SoupKitchen where

import           System.Random

import           Canonical

defaultGen :: StdGen
defaultGen = mkStdGen 144

randCGen :: RandomGen g => g -> (Int, g)
randCGen = 
    randomR (fromEnum (minBound::Canonical), fromEnum (maxBound::Canonical))
