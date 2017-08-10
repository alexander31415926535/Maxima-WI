-- TODO: Add haskeline support , to have history.
-- * Imports , pragmas, datatypes
--   Emacs: To enable org functionality start orgstruct-mode , then use TAB , S-TAB on headlines : -- * Title.

{-# LANGUAGE LambdaCase #-}

module Maxima ( MaximaServerParams
              , runMaxima
              , askMaxima
              , askMaximaRaw
              , startMaximaServer
              , initMaximaVariables
              )
  where

import Data.List
import Data.Char
import Network.Socket
import System.IO
import System.Process
import System.Posix.Signals
import System.Process.Internals
import Control.Exception
import Control.Concurrent

data MaximaServerParams = MaximaServerParams 
  { mConnection :: Socket
  , mSocket :: Socket
  , mHandle :: Handle
  , mPid :: ProcessHandle 
  }

-- * Functions
-- ** Server communication & setup
-- *** startMaximaServer
startMaximaServer port = withSocketsDo $ do
    conn          <-  listenServer port
    (_,_,_,pid)   <-  runInteractiveProcess "maxima"
                                            ["-r", ":lisp (setup-client "++show port++")"]--there is also -s option to connect to port,why lisp?
                                            Nothing Nothing
    (sock, _)     <-  accept conn
    socketHandle  <-  socketToHandle sock ReadWriteMode
    hSetBuffering     socketHandle        NoBuffering
    _             <-  hTakeWhileNotFound "(%i" socketHandle >>
                      hTakeWhileNotFound ")"   socketHandle
    return $ MaximaServerParams conn sock socketHandle pid

-- *** listenServer 
listenServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock  ReuseAddr 1
    bind            sock (SockAddrInet port iNADDR_ANY)
    listen sock 1
    return sock

-- *** hTakeWhileNotFound
hTakeWhileNotFound str hdl = reverse <$> findStr str hdl [0] []

  where findStr st hl indices acc = do 
         c <- hGetChar hl
         let newIndeces = [ i+1 | i <- indices, i < length st, st!!i == c]
         if length st `elem` newIndeces
           then return (c : acc)
           else findStr str hdl (0 : newIndeces) (c : acc)



-- ** Maxima ask/response

askMaximaRawOld (MaximaServerParams _ _ hdl _) question = do
    hPutStrLn hdl question
    result <- hTakeWhileNotFound "(%i" hdl
    _      <- hTakeWhileNotFound ")" hdl
    return $ take (length result - 3) result

askMaximaRaw (MaximaServerParams _ _ hdl _) question = do
    hPutStrLn hdl question
    -- result <- hTakeWhileNotFound "(%i" hdl
    -- _      <- hTakeWhileNotFound ")" hdl
    -- (++) <$> hGetLine hdl <*> hGetLine hdl -- Applicative instance of monad
    result <- hTakeWhileNotFound "\n(%i" hdl -- Small change here to allow multipage support
    _      <- hTakeWhileNotFound ")" hdl
    return $ take (length result - 3) result
        


initMaximaVariables maxima = do
    _  <- askMaximaRawOld maxima "display2d: false;"
    askMaximaRawOld       maxima "linel: 10000;"

askMaxima maxima question = 
  if null $ dropWhile isSpace question 
     then return []
     else do
       let q = dropWhileEnd isSpace question
           q2 = if last q `elem` ['$',';'] then q else q ++ ";"
       result <- askMaximaRaw maxima q2
       return $
         -- filter (not . null) . map (drop 2) . filter (not . null) . map (dropWhile (/=')')) $
         result
    
-- ** Maxima process management

runMaxima port f = bracket (startMaximaServer port)
                           (\srv -> do terminateProcess2 (mPid srv)
                                       _  <- waitForProcess (mPid srv)
                                       close (mConnection srv))
                           (\x -> initMaximaVariables x >> f x)

 

terminateProcess2 :: ProcessHandle -> IO ()
terminateProcess2 (ProcessHandle pmvar _ _) = 
    readMVar pmvar >>= \case 
        OpenHandle pid -> signalProcess 15 pid -- pid is a POSIX pid
        _              -> return () -- hlint suggested to remove otherwise here : Used otherwise as a pattern
    
