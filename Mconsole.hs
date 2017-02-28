#!/usr/bin/runghc 

{-# LANGUAGE MultiWayIf #-}
-- * Mconsole
-- This module attempts to introduce unicode goodies into maxima text console. things like x^2 are written in proper unicode.

-- * Imports
module Mconsole where

import Data.Attoparsec.ByteString.Char8 
import Data.ByteString.Char8 (pack) 
import Maxima
import Data.List.Extra hiding (any)
import System.Console.Haskeline
import Control.Monad.Trans (lift)

-- * tounicode function
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

commlistE :: [String]
commlistE = [
  "example",
  "exp",
  "expand",
  "expandwrt",
  "expandwrt_denom",
  "expandwrt_factored",
  "expintegral_chi",
  "expintegral_ci",
  "expintegral_e",
  "expintegral_e1",
  "expintegral_e_simplify",
  "expintegral_ei",
  "expintegral_li",
  "expintegral_shi",
  "expintegral_si",
  "expintexpand",
  "expintrep",
  "explicit",
  "explose",
  "expon",
  "exponentialize",
  "exponentialize",
  "expop",
  "express",
  "expt",
  "exptdispflag",
  "exptisolate",
  "exptsubst",
  "exsec",
  "extdiff",
  "extract_linear_equations",
  "extremal_subset",
  "ezgcd"]

mcompletion :: CompletionFunc IO
mcompletion = completeWord Nothing [' '] helper
  where
    helper :: String -> IO [Completion]
    helper x | length x >= 1 = return (map simpleCompletion (filter (isPrefixOf x) commlistE))
             | otherwise = return [simpleCompletion "help"]


