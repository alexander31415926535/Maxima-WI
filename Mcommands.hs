#!/usr/bin/local/runghc 

import Text.XML.HXT.Core

prefix = "/usr/share/maxima/5.38.1/doc/html/maxima_" -- 89 --104 this url changes for different maxima versions

fullindex = map (\x -> prefix ++ show x ++ ".html") [89 .. 104]

main :: IO ()
main = do
  html <- mapM readFile fullindex
  commands <- runX $ readString [withParseHTML yes, withWarnings no] (mconcat html)
    //> hasName "code"
    //> getText
  writeFile "all-maxima-commands.txt" (show commands)
