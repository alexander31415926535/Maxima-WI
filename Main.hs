#!/usr/local/bin/runghc 
-- * Imp
-- ** Pragmas
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- ** Imports
module Main where

import Util
import Maxima
import qualified Prelude as P 
import Data.IORef
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

-- * Html render with lucid, types
data HTMLLucid
instance Accept HTMLLucid                          where contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a        where  mimeRender _ = renderBS . toHtml
instance             MimeRender HTMLLucid (Html a) where  mimeRender _ = renderBS
-- * Main
-- ** Types,Instances and APIs
type MaximaAPI = QueryParam "maximainput" String :> Get '[JSON, HTMLLucid] Form
                 :<|> Get '[JSON, HTMLLucid] Form -- root path

data Form = Form { greeting :: Text , action :: Text , svgplot :: Text } deriving Generic ; instance ToJSON Form

instance ToHtml Form where   -- XXX: here rethinking is needed and new data structure (maybe greeting  -> [Text])
  toHtml form = printedtext <> plot <> formitself
    where
    printedtext = ol_ $ mconcat (map (li_ . toHtmlRaw) (lines (greeting form)))
    plot        = (toHtmlRaw (svgplot form))
    formitself  = form_ [formaction_ (action form), formmethod_ "GET" ] (do
                           toHtml ("Input:"::Text)
                           input_ [name_ "maximainput",type_ "text", autofocus_]
                           input_ [type_ "submit"])
  toHtmlRaw = toHtml

instance Monoid Form where
  mempty        = Form "" "" ""
  mappend f1 f2 = Form (greeting f1 <> greeting f2) (action f1) (svgplot f2)
-- ** Parsing plot commands Svg output

-- XXX: Plotting can be done using maxima macro :  plotwi(x,y) ::= plot2d(x,y,[svg_file,"maxima-wi-communication.svg"])
-- XXX: 
-- XXX: USE most general functions (applicative,monad operators) to make it EASY to switch main data structures.
-- XXX: 
(<^>) = flip (<?>)              -- more convenient help combinator

svgfilep :: A.Parser B.ByteString
svgfilep = "Svg file parser" <^> skipWhile (/='[') *> skipWhile (/=',') *> skipWhile (/='\"') *> char '\"' *> A.takeWhile (/='\"')

isplot :: String -> Bool
isplot x = case parseOnly isplotp (B.pack x) of
             Left _ -> False
             Right _ -> True
           where isplotp = "Is input a plot command" <^> string "plotwi" <* takeByteString  

findsvg :: String -> Maybe String
findsvg x = case parseOnly svgfilep (B.pack x) of
                  Left _ -> Nothing
                  Right r -> Just (B.unpack r)

-- ** Main algorythm

-- Maxima answer is of the form " \n(%o3)....\n"

mkform str plot    =  Form (pack str) "maximaquery" (pack plot) ; formone  =  Form "Maxima input here: " "maximaquery" ""

answerMech i o     = if isplot i  then  case findsvg o of Nothing       -> "Could not recognize filename"
                                                          Just svgfname -> astr i svgfname
                                  else     astr i o 
                   where                   astr al be = mconcat ["(%i) " , al , "\n" , be] -- answer string
   
formhandler p ior x =
                case x of Nothing -> return formone                             -- Just a below is result returned by servant -- an input string
                          Just a  -> do manswer@(_:_) <- liftIO (askMaxima p a) -- here take whole argument not just first element
                                        log1          <- liftIO (readIORef ior) --- DANGER!!! 
                                        let ma         = (P.unlines . tail . P.lines) manswer --remove first line in maxima output which is -> " \n(%o34) ..."
                                        let ma1        = answerMech a ma                      -- check if input a is a plot command
                                        let newlog1    = log1 <> mkform ma1 ""
                                        liftIO (writeIORef ior newlog1)
                                        case findsvg ma of
                                         Nothing       -> return newlog1
                                         Just svgfpath -> liftIO (readFile svgfpath) >>= \svgg -> return (log1 <> mkform ma1 svgg)
  

                    

-- ** Server and main

maximaAPI                     = Proxy                                          :: Proxy MaximaAPI 
server   p flog               = formhandler p flog :<|> return formone         -- :: (MaximaServerParams  -> IORef Form-> Server MaximaAPI) :<|> Server MaximaAPI

main = do  params <- startMaximaServer 4424
           _      <- initMaximaVariables params
           flog   <- newIORef (mkform "" "")                            -- XXX: DANGER!!! IORefS!!
           let app (p :: MaximaServerParams) = serve maximaAPI (server p flog) :: Application -- classic variable passing in argument
           _      <- askMaxima params "plotwi (x,y)::= plot2d(x,y,[svg_file,\"maximawi-plot.svg\"])" -- setting plot macro
           putStrLn "Maxima and Server started." 
           run 8081 (app params )




