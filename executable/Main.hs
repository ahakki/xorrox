{-# OPTIONS_GHC -Wno-orphans #-}

module Main where
import Alphabets
import Canonical

import Data.Char (toUpper)
import Data.Maybe (fromJust)

main :: IO()
main = do
  putStrLn "Spaces are displayed as underscores (_). The ending of a line is indicated by a tilde (~). Any parse errors are displyed as a hash (#)."
  putStrLn "Mode selection: Encrypt or Decrypt (E/D) (default E): "
  mode <- getLine
  if map toUpper mode /= "D" && map toUpper mode /= "E" && mode /= "" then
    main
  else do
    putStrLn "Password:"
    pw <- getLine
    let y = readHeresies pw
    if notElem Nothing y then do
      let password = Password $ map fromJust y
      if map toUpper mode == "D" then
        decrypt password
      else encrypt password
    else do print y; main

choose :: IO()
choose = do
  putStrLn "Mode selection: Encrypt or Decrypt (E/D) (default E): "
  mode <- getLine
  if map toUpper mode /= "D" && map toUpper mode /= "E" && mode /= "" then
    choose
  else do
    putStrLn "Password:"
    pw <- getLine
    let y = readHeresies pw
    if Nothing `notElem` y then do
      let password = Password $ map fromJust y
      if map toUpper mode == "D" then
        decrypt password
      else encrypt password
    else do print y; main


encrypt :: Password -> IO()
encrypt pw = do
  putStrLn "Message to ENCRYPT:"
  x <- getLine
  let y = readHeresies x
  if Nothing `notElem` y then do
    let a = Clear $ VLine $ map fromJust y
    print (vigengL (passmatrix (passlist canonicallist pw)) a pw)
    main
    print a
  else do
    print y
    main

decrypt :: Password -> IO()
decrypt pw = do
  putStrLn "Message to DECRYPT:"
  x <- getLine
  let y = readHeresies x
  if Nothing `notElem` y then do
    let a = Hidden $ VLine $ map fromJust y
    print (gnegivL (passmatrix (passlist canonicallist pw)) a pw)
    main
    print a
  else do
    print y
    main


