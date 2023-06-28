{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib (runP4) where

import Control.Exception (bracket)
import Control.Monad (guard, liftM2, unless, (>=>))
import Data.ByteString (ByteString, packCString)
import Foreign.C.String (newCString, peekCAString, peekCString, withCAString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal (maybePeek, toBool)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr (nullPtr)
import qualified Language.C.Inline.Cpp as C
import System.Console.ANSI
import Prelude hiding (init)

data ClientAPI

data P4Env = P4Env
  { p4user :: Maybe String,
    p4password :: Maybe String,
    p4host :: Maybe String,
    p4port :: Maybe String,
    p4client :: Maybe String
  }

newtype P4 = P4 (ForeignPtr ClientAPI)

data P4Handler
  = OutputText (String -> IO ())
  | OutputInfo (String -> IO ())
  | OutputMessage (String -> IO ())
  | OutputStat (String -> IO ())
  | OutputBinary (ByteString -> IO ())

instance Show P4Handler where
  show (OutputText _) = "outputText"
  show (OutputInfo _) = "outputInfo"
  show (OutputMessage _) = "outputMessage"
  show (OutputStat _) = "outputStat"
  show (OutputBinary _) = "outputBinary"

C.context
  ( C.cppCtx
      <> C.baseCtx
      <> C.fptrCtx
      <> C.funCtx
      <> C.cppTypePairs [("HsClientApi", [t|ClientAPI|])]
  )

C.include "hsclientapi.h"

runP4 :: [String] -> IO ()
runP4 = runP4Env defaultP4Env >=> either (colored Red) (colored Green)

runP4Env :: P4Env -> [String] -> IO (Either String String)
runP4Env _ [] = return $ Left "p4h\n"
runP4Env env (cmd : arg) = withP4Env env $ \p4 -> setArgv p4 arg >> run p4 cmd >> getOutput2 p4

defaultP4Env :: P4Env
defaultP4Env = P4Env Nothing Nothing Nothing Nothing Nothing

connectEnv :: P4Env -> IO P4
connectEnv (P4Env user pass host port client) = do
  p4 <- newP4
  may (setClient p4) client
  may (setHost p4) host
  may (setPassword p4) pass
  may (setPort p4) port
  may (setUser p4) user
  connect p4
  return p4
  where
    may = maybe (return ())

disconnect :: P4 -> IO ()
disconnect (P4 fpClient) = [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->Disconnect(); } |]

withP4Env :: P4Env -> (P4 -> IO a) -> IO a
withP4Env env = bracket (connectEnv env) disconnect

newP4 :: IO P4
newP4 = do
  ptrClient <- [C.exp| HsClientApi* { new HsClientApi } |]
  let finalizer = [C.funPtr| void free(HsClientApi *p) { delete p; } |]
  P4 <$> newForeignPtr finalizer ptrClient

setHandler :: P4 -> P4Handler -> IO ()
setHandler (P4 fpClient) handler = do
  let out = get handler
  withCAString (show handler) $ \name ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetHandler($(const char *name), $fun-alloc:(void (*out)(const char *))); } |]
  where
    get (OutputBinary h) = packCString >=> h
    get (OutputInfo h) = peekCAString >=> h
    get (OutputText h) = peekCAString >=> h
    get (OutputMessage h) = peekCAString >=> h
    get (OutputStat h) = peekCAString >=> h

setPort :: P4 -> String -> IO ()
setPort (P4 fpClient) port' = withCAString port' $ \port ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetPort($(const char *port)); } |]

setUser :: P4 -> String -> IO ()
setUser (P4 fpClient) user' = withCAString user' $ \user ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetUser($(const char *user)); } |]

setPassword :: P4 -> String -> IO ()
setPassword (P4 fpClient) password' = withCAString password' $ \password ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetPassword($(const char *password)); } |]

setHost :: P4 -> String -> IO ()
setHost (P4 fpClient) host' = withCAString host' $ \host ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetHost($(const char *host)); } |]

setClient :: P4 -> String -> IO ()
setClient (P4 fpClient) client' = withCAString client' $ \client ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetClient($(const char *client)); } |]

setInput :: P4 -> String -> IO ()
setInput (P4 fpClient) inp' = withCAString inp' $ \inp ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetInput($(const char *inp)); } |]

setArgv :: P4 -> [String] -> IO ()
setArgv (P4 fpClient) args = unless (null args) $
  bracket (mapM newCString args) (mapM_ free) $ \argv' -> withArrayLen argv' $ \argc' argv -> do
    let argc = fromIntegral argc'
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetArgv($(int argc), $(char *const *argv)); } |]

setProg :: P4 -> String -> IO ()
setProg (P4 fpClient) prog' = withCAString prog' $ \prog ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetProg($(const char *prog)); } |]

setVersion :: P4 -> String -> IO ()
setVersion (P4 fpClient) version' = withCAString version' $ \version ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetVersion($(const char *version)); } |]

setProtocol :: P4 -> String -> String -> IO ()
setProtocol (P4 fpClient) var val = withCAString var $ \p -> withCAString val $ \v ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetProtocol($(const char *p), $(const char *v)); } |]

getProtocol :: P4 -> String -> IO (Maybe String)
getProtocol (P4 fpClient) var = withCAString var $ \p -> do
  pv <-
    [C.block| const char * { 
        StrPtr *pv = $fptr-ptr:(HsClientApi *fpClient)->GetProtocol($(const char *p));
        return pv ? pv->Text() : nullptr;
      }
    |]
  maybePeek peekCAString pv

dropped :: P4 -> IO Bool
dropped (P4 fpClient) = toBool <$> [C.block| int { return $fptr-ptr:(HsClientApi *fpClient)->Dropped(); } |]

connect :: P4 -> IO ()
connect (P4 fpClient) = [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->Connect(); } |]

run :: P4 -> String -> IO ()
run (P4 fpClient) cmd' = withCAString cmd' $ \cmd ->
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->Run($(const char *cmd)); } |]

getOutput2 :: P4 -> IO (Either String String)
getOutput2 (P4 fpClient) = bracket getPtrs freePtrs $ \(msg, err) -> do
  guard (msg /= nullPtr && err /= nullPtr)
  liftM2 ret (peekCString msg) (peekCString err)
  where
    getPtrs = C.withPtrs_ $ \(msg, err) ->
      [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->GetOutput2($(const char **msg), $(const char **err)); } |]
    freePtrs (p1, p2) = free p1 >> free p2
    ret a "" = Right a
    ret _ b = Left b

parseSpec :: P4 -> String -> String -> IO [(String, String)]
parseSpec (P4 fpClient) typ' form' = withCAString typ' $ \typ -> withCAString form' $ \form -> do
  (k, v, i) <- C.withPtrs_ $ \(k, v, i) ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->ParseSpec($(const char *typ), $(const char *form), $(const char ***k), $(const char ***v), $(int *i)); } |]
  let len = fromIntegral i
  dict <- liftM2 zip (process k len) (process v len)
  free k >> free v >> return dict
  where
    process arr len = bracket (peekArray len arr) (mapM_ free) (mapM peekCAString)

colored :: Color -> String -> IO ()
colored clr txt = do
  setSGR [SetColor Foreground Vivid clr]
  putStr txt
  setSGR [Reset]
