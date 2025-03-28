module Main where

import           Fileinterface (fileinterface)
import           Lineinterface (lineinterface)

import           Data.Char     (toUpper)

main :: IO()
main = do
    putStrLn "Function selection: Inline or File (I/F) (default I): "
    mode <- getLine
    if map toUpper mode /= "F" && map toUpper mode /= "I" && mode /= "" 
    then main
    else do
      if map toUpper mode == "F" then
        fileinterface
      else lineinterface
