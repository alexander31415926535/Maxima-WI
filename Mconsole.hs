#!/usr/local/bin/runghc 

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- * Mconsole
-- This module attempts to introduce unicode goodies into maxima text console. things like x^2 are written in proper unicode.

-- * TODO
-- TODO: Add command history between sessions

-- * Imports
module Main where

import Data.Attoparsec.ByteString.Char8 
import Data.ByteString.Char8 (pack) 
import Maxima
import Data.List.Extra hiding (any)
import System.Console.Haskeline
import Control.Monad.Trans (lift)
import Mcommlist

-- * tounicode function and parsers
tounicode :: String -> String
tounicode str = foldl1 (.) (zipWith replace  ("*":terms) ("·":helper terms)) str
  where terms = case parseOnly allpowers (pack str) of
                 Left _     -> []
                 Right pstr -> pstr
        helper xp = map (foldl1 (.) (zipWith  replace ["^","1","2","3","4","5","6","7","8","9","0"]
                                                      ["","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹","⁰"])) xp

(<^>) = flip (<?>)              -- more convenient help combinator

powerp :: Parser String
powerp = "Powerp"  <^> ((:) <$> char '^' <*> many1 digit)

allpowers :: Parser [String]
allpowers = "allpowers"  <^> many' (takeTill (== '^') *> powerp)

outputLnumber :: Parser Int
outputLnumber = "Output sequence parser (%oN)" <^> string "(%o" *> decimal <* char ')'

mxNumber :: Parser Int
mxNumber = "Parsing maxima single number output" <^> outputLnumber *> many1 space *> decimal

-- * main             

-- oldmain = runMaxima 4424 maximaPrompt

main :: IO ()
main = do
  putStrLn "Maxima\nDedicated to the memory of William Schelter"
  runMaxima 4424 haskelinemod

haskelinemod srv = runInputT (setComplete mcompletion defaultSettings) loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just "help"  -> do
         lift $ listcommands
         loop
       Just input -> do
         answer <- lift $ askMaxima srv input
         let ans = tounicode (if length answer > 4 then init (drop 2 answer) else answer) -- XXX: ugly code to remove " \n" on both ends.
         outputStrLn $ ans
         loop

listcommands :: IO ()
listcommands = do
  mapM_ putStrLn ["Following commands are active:",
                  "help",
                  "quit"]

-- * Completion, big lists

mcompletion :: CompletionFunc IO
mcompletion = completeWord Nothing [' '] helper
  where
    helper :: String -> IO [Completion]
    helper x | length x >= 1 = return (map simpleCompletion (filter (isPrefixOf x) mcommands))
             | otherwise = return [simpleCompletion "help"]


-- * Separate Functions for Ghci debug and Integration

amu srv input = do
  answer <- askMaxima srv input
  let ans = tounicode (if length answer > 4 then init (drop 2 answer) else answer) -- XXX: ugly code to remove " \n" on both ends.
  putStrLn ans

envarg f s = f++"("++show s++")"     -- envelope argument
envarg0 f s = f++show s              -- envelope argument

factorial :: MaximaServerParams -> Int -> IO (Maybe Int)
factorial srv x = do
    answer <- fmap (head . tail . lines) $ askMaxima srv (envarg "factorial" x)
    case parseOnly mxNumber (pack answer) of
         Left _     -> return Nothing
         Right numbr -> return (Just numbr)
  
binomial :: MaximaServerParams -> (Int,Int) -> IO (Maybe Int)
binomial srv x = do
    answer <- fmap (head . tail . lines) $ askMaxima srv (envarg0 "binomial" x)
    case parseOnly mxNumber (pack answer) of
         Left _     -> return Nothing
         Right numbr -> return (Just numbr)

makeMF1 :: MaximaServerParams  -> String -> (Int -> IO (Maybe Int)) -- general function maker
makeMF1 srv fname = \x ->  do
    answer <- fmap (head . tail . lines) $ askMaxima srv (envarg fname x)
    case parseOnly mxNumber (pack answer) of
         Left _     -> return Nothing
         Right numbr -> return (Just numbr)


makeMF2 :: MaximaServerParams  -> String -> ((Int,Int) -> IO (Maybe Int)) -- general function maker
makeMF2 srv fname = \x ->  do
    answer <- fmap (head . tail . lines) $ askMaxima srv (envarg0 fname x)
    case parseOnly mxNumber (pack answer) of
         Left _     -> return Nothing
         Right numbr -> return (Just numbr)

makeMF3 :: MaximaServerParams  -> String -> ((Int,Int,Int) -> IO (Maybe Int)) -- general function maker
makeMF3 srv fname = \x ->  do
    answer <- fmap (head . tail . lines) $ askMaxima srv (envarg fname x)
    case parseOnly mxNumber (pack answer) of
         Left _     -> return Nothing
         Right numbr -> return (Just numbr)

