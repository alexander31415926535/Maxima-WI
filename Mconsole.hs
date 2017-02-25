{-# LANGUAGE MultiWayIf #-}

module Mconsole
    ( maximaPrompt 
    ) where

import Data.Attoparsec.ByteString.Char8 
import Data.ByteString.Char8 (pack) 
import Maxima
import Data.List.Extra hiding (any)
-- TODO: Add haskeline support, to have history etc.
  
maximaPrompt srv = do
    putStr "> "
    question <- getLine
    if
      | question == ":q" -> return ()
      | question == ":h" ->
        do putStrLn "Type maxima command or :q to quit."
           maximaPrompt srv
      | otherwise -> do answer <- askMaxima srv question
                        mapM_ (putStrLn . tounicode) answer
                        maximaPrompt srv
               

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
         
main = runMaxima 4424 maximaPrompt
