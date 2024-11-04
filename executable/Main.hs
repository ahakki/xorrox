{-# LANGUAGE LambdaCase   #-}

module Main where

import Classical
import Canonical
import Heretical
import Alphabets
import SoupKitchen

import           Heretical (Heretical (..), escape, rescape)
import           Alphabets (Password, canonicallist, gnegivL, mkCleartext,
                            mkHiddentext, mkPassword, passline, passmatrix,
                            vigengL)
import           Data.Char (toUpper)

main :: IO()
main = do
  putStrLn "Spaces are displayed as underscores (_). The ending of a line is indicated by a hashmark (#) and can be entered as such. Any parse errors are displyed as a tilde (~)."
  choose

getLineAndRescape :: IO String
getLineAndRescape = getLine >>= \x -> return $ rescape x

showParseErrors :: Heretical a => [a] -> IO ()
showParseErrors x = print $ readHeresies x

choose :: IO()
choose = do
  putStrLn "Mode selection: Encrypt or Decrypt (E/D) (default E): "
  mode <- getLine
  if map toUpper mode /= "D" && map toUpper mode /= "E" && mode /= "" then
    choose
  else do
    putStrLn "Password:"
    pw <- getLineAndRescape
    if readableHeresies pw then do
      let password = mkPassword pw
      if map toUpper mode == "D" then
        decrypt password
      else encrypt password
    else do showParseErrors pw; choose

encrypt :: Alphabets.Password -> IO()
encrypt pw = do
  putStrLn "Message to ENCRYPT:"
  x <- getLineAndRescape
  if readableHeresies x then do
    let a = mkCleartext x
    print $ hide pw a
    choose
  else do showParseErrors x; encrypt pw

decrypt :: Alphabets.Password -> IO()
decrypt pw = do
  putStrLn "Message to DECRYPT:"
  x <- getLineAndRescape
  if readableHeresies x then do
    let a = mkHiddentext x
    print $ reveal pw a
    choose
  else do showParseErrors x; decrypt pw