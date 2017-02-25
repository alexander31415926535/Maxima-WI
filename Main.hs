#!/usr/bin/runghc 
-- * Imp
-- ** Pragmas
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
-- ** Imports
module Main where
import Util
import Maxima
import qualified Prelude as P 
import Data.IORef

-- * Html render with lucid, types
data HTMLLucid
instance Accept HTMLLucid                          where contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a        where  mimeRender _ = renderBS . toHtml
instance             MimeRender HTMLLucid (Html a) where  mimeRender _ = renderBS
-- * Main
-- ** Types and APIs
type MaximaAPI = "maxima" :> QueryParam "maximainput" String :> Get '[JSON, HTMLLucid] Form

data Form = Form { greeting :: Text , action :: Text } deriving Generic ; instance ToJSON Form

instance ToHtml Form where
  toHtml form = printedtext <> formitself
    where
    printedtext = ol_ $ mconcat (map (li_ . toHtmlRaw) (lines (greeting form)))
    formitself  = form_ [formaction_ (action form), formmethod_ "GET" ] (do
                           toHtml ("Input:"::Text)
                           input_ [name_ "maximainput",type_ "text", autofocus_]
                           input_ [type_ "submit"])
  toHtmlRaw = toHtml

instance Monoid Form where
  mempty        = Form "" ""
  mappend f1 f2 = Form (greeting f1 <> greeting f2) (action f1)

-- ** Main algorythm

formone    =  Form "Maxima input here: " "maximaquery" 

form12 s   =  Form s "maximaquery" -- s :: String
form2 p ior x = case x of Nothing -> return formone
                          Just a  -> do ma@(manswer:_) <- liftIO (askMaxima p a) -- here take whole argument not just first element
                                        log1 <- liftIO (readIORef ior)               --- DANGER!!! 
                                        let ma1 = "(%i) " ++ a ++ "\n" ++ (P.unlines . tail . P.lines) ma -- a is input string
                                        let newlog1 = log1 <> form12 (pack ma1) 
                                        liftIO (writeIORef ior newlog1)
                                        return (log1 <> form12 (pack ma1))


maximaAPI                     = Proxy                      :: Proxy MaximaAPI 
server                        = form2                      :: (MaximaServerParams  -> IORef Form-> Server MaximaAPI)

main = do  params <- startMaximaServer 4424
           _      <- initMaximaVariables params
           flog   <- newIORef (form12 "")                            -- XXX: DANGER!!! IORefS!!
           let app (p :: MaximaServerParams) = serve maximaAPI (server p flog) :: Application -- classic variable passing in argument
           -- _      <- askMaxima params "set_tex_environment_default (\"\", \"\")" -- setting correct output format
           putStrLn "Maxima and Server started." 
           run 8081 (app params )

