module Fileinterface (fileinterface) where

import           Alphabets

import qualified Data.ByteString as BS
import           Classical (reveal8)

fileinterface :: IO()
fileinterface = do
    pw <- getLine
    filepath <- getLine
    input <- BS.readFile filepath
    let output = reveal8 pw input
    
    print output
    BS.writeFile "output" output