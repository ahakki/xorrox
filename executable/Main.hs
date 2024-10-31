module Main (main) where

import Alphabets
    ( canonicallist,
      Password,
      passlist,
      passmatrix,
      vigengL,
      gnegivL,
      rescape,
      mkPassword,
      mkCleartext,
      mkHiddentext )

import Canonical ( Heretical(readHeresies) )

import Data.Char (toUpper)

main :: IO()
main = do
  putStrLn "Spaces are displayed as underscores (_). The ending of a line is indicated by a tilde (~) and can be entered as such. Any parse errors are displyed as a hash (#)."
  choose

showParseErrors :: Heretical a => [a] -> IO ()
showParseErrors x = print (readHeresies x)

choose :: IO()
choose = do
  putStrLn "Mode selection: Encrypt or Decrypt (E/D) (default E): "
  mode <- getLine 
  if map toUpper mode /= "D" && map toUpper mode /= "E" && mode /= "" then
    choose
  else do
    putStrLn "Password:"
    pw <- getLine
    if notElem Nothing $ readHeresies $ rescape pw then do
      let password = mkPassword $ rescape pw
      if map toUpper mode == "D" then
        decrypt password
      else encrypt password
    else do showParseErrors pw; choose


encrypt :: Password -> IO()
encrypt pw = do
  putStrLn "Message to ENCRYPT:"
  x <- getLine
  if notElem Nothing $ readHeresies $ rescape x then do
    let a = mkCleartext $ rescape x
    print (vigengL (passmatrix (passlist canonicallist pw)) a pw)
    choose
  else do showParseErrors x; encrypt pw

decrypt :: Password -> IO()
decrypt pw = do
  putStrLn "Message to DECRYPT:"
  x <- getLine
  if notElem Nothing $ readHeresies $ rescape x then do
    let a = mkHiddentext $ rescape x
    print (gnegivL (passmatrix (passlist canonicallist pw)) a pw)
    choose
  else do showParseErrors x; decrypt pw


