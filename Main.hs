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
import Prelude hiding (lines)
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

-- XXX: add here foldMap toHtml to generate list of results not just one
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

form1    =  Form "Maxima input here: " "maximaquery" 
form12 s =  Form s "maximaquery" -- s :: String
form2 p ior x = case x of Nothing -> return form1 
                          Just a  -> do ma@(manswer:_) <- liftIO (askMaxima p a) -- here take whole argument not just first element
                                        log1 <- liftIO (readIORef ior)               --- DANGER!!! 
                                        let newlog1 = log1 <> form12 (pack ma) 
                                        liftIO (writeIORef ior newlog1)
                                        return (log1 <> form12 (pack ma))
                                    -- let str = (replace "\\\\" "\\" . filter (\el -> el /='\n' && el /='\"')) manswer
                                    -- let tex = readTeX str
                                    -- case tex of Left  errr    ->  do _ <- liftIO (putStrLn errr) ; return (form12 manswer)
                                    --             Right reply -> do  let manswerML = showElement (writeMathML DisplayInline reply)
                                    --                                return (form12 manswerML)
maximaAPI                     = Proxy                      :: Proxy MaximaAPI 
server                        = form2                      :: (MaximaServerParams  -> IORef Form-> Server MaximaAPI)
-- app (p :: MaximaServerParams) = serve maximaAPI (server p) :: Application -- classic variable passing in argument


main = do  params <- startMaximaServer 4424
           _      <- initMaximaVariables params
           log <- newIORef (form12 " ")
           let app (p :: MaximaServerParams) = serve maximaAPI (server p log) :: Application -- classic variable passing in argument
           -- _      <- askMaxima params "set_tex_environment_default (\"\", \"\")" -- setting correct output format
           putStrLn "Maxima and Server started." 
           run 8081 (app params )

