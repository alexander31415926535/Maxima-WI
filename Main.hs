#!/usr/bin/runghc 
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
type MaximaAPI = "maxima" :> QueryParam "maximainput" String :> Get '[JSON, HTMLLucid] Form

data Form = Form { greeting :: Text , action :: Text , svgplot :: Text } deriving Generic ; instance ToJSON Form

instance ToHtml Form where
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

-- ** Main algorythm
-- ** Parsing plot commands Svg output


-- XXX: Plotting can be done using maxima macro :  plotwi(x,y) ::= plot2d(x,y,[svg_file,"maxima-wi-communication.svg"])

(<^>) = flip (<?>)              -- more convenient help combinator

svgfilep :: A.Parser B.ByteString
svgfilep = "Svg file parser" <^> skipWhile (/='[') *> skipWhile (/=',') *> skipWhile (/='\"') *> char '\"' *> A.takeWhile (/='\"')

isplot :: String -> Bool
isplot x = case parseOnly isplotp (B.pack x) of
             Left _ -> False
             Right _ -> True
           where isplotp = "Is input a plot command" <^> string "plotwi" <* takeByteString  

svgfile :: String -> Maybe String
svgfile x = case parseOnly svgfilep (B.pack x) of
              Left _ -> Nothing
              Right r -> Just (B.unpack r)
                 
  
-- ** Form and main

formone    =  Form "Maxima input here: " "maximaquery" ""

-- Maxima answer is of the form " \n(%o3)....\n"
form12 s plot =  Form s "maximaquery" plot 
form2 p ior x = case x of Nothing -> return formone -- a is result returned by servant -- an input string
                          Just a  -> do ma@(manswer:_) <- liftIO (askMaxima p a) -- here take whole argument not just first element
                                        log1 <- liftIO (readIORef ior)               --- DANGER!!! 
                                        let ma1 = if isplot a then case (svgfile ((P.unlines . tail . P.lines) ma)) of
                                                                     Nothing -> "Could not recognize filename"
                                                                     Just svgp -> "(%i) " ++ a ++ "\n" ++ svgp
                                                              else "(%i) " ++ a ++ "\n" ++ (P.unlines . tail . P.lines) ma -- a is input string
                                        case (svgfile ((P.unlines . tail . P.lines) ma)) of
                                                                     Nothing -> do
                                                                                  let newlog1 = log1 <> form12 (pack ma1) ""
                                                                                  liftIO (writeIORef ior newlog1)
                                                                                  return (log1 <> (form12 (pack ma1) ""))
                                                                     Just svgfpath -> do
                                                                       svgcontent <- liftIO (readFile svgfpath)
                                                                       let newlog1 = log1 <> form12 (pack ma1) ""
                                                                       liftIO (writeIORef ior newlog1)
                                                                       return (log1 <> (form12 (pack ma1) (pack svgcontent)))



maximaAPI                     = Proxy                      :: Proxy MaximaAPI 
server                        = form2                      :: (MaximaServerParams  -> IORef Form-> Server MaximaAPI)

main = do  params <- startMaximaServer 4424
           _      <- initMaximaVariables params
           flog   <- newIORef (form12 "" "")                            -- XXX: DANGER!!! IORefS!!
           let app (p :: MaximaServerParams) = serve maximaAPI (server p flog) :: Application -- classic variable passing in argument
           -- _      <- askMaxima params "set_tex_environment_default (\"\", \"\")" -- setting correct output format
           _      <- askMaxima params "plotwi (x,y)::= plot2d(x,y,[svg_file,\"maximawi-plot.svg\"])" -- setting plot macro
           putStrLn "Maxima and Server started." 
           run 8081 (app params )



