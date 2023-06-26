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
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline.Cpp (withPtrs_)
import qualified Language.C.Inline.Cpp as C
import Prelude hiding (init)

data ClientAPI

data ClientUser

data P4Env = P4Env
  { p4user :: Maybe String,
    p4password :: Maybe String,
    p4host :: Maybe String,
    p4port :: Maybe String,
    p4client :: Maybe String,
    p4input :: Maybe String
  }

data P4 = P4 (ForeignPtr ClientAPI) (ForeignPtr ClientUser)

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
      <> C.cppTypePairs
        [ ("HsClientApi", [t|ClientAPI|]),
          ("HsClientUser", [t|ClientUser|])
        ]
  )
C.include "hsclientapi.h"
C.include "hsclientuser.h"

runP4 :: [String] -> IO (Either String String)
runP4 = flip (runP4Env defaultP4Env) (const $ return ())

defaultP4Env :: P4Env
defaultP4Env = P4Env Nothing Nothing Nothing Nothing Nothing Nothing

connectEnv :: P4Env -> IO P4
connectEnv (P4Env user pass host port client input) = do
  p4 <- newP4
  may (setUser p4) user
  may (setPassword p4) pass
  may (setHost p4) host
  may (setPort p4) port
  may (setClient p4) client
  may (setInput p4) input
  init p4
  return p4
  where
    may io (Just a) = io a
    may _ _ = return ()

disconnect :: P4 -> IO ()
disconnect (P4 fpClient fpUi) =
  [C.block| void {
      Error e;
      $fptr-ptr:(HsClientApi *fpClient)->Final(&e);
      if (e.Test())
        $fptr-ptr:(HsClientUser *fpUi)->HandleError(&e);
    }
  |]

withP4Env :: P4Env -> (P4 -> IO a) -> IO a
withP4Env env = bracket (connectEnv env) disconnect

runP4Env :: P4Env -> [String] -> (P4 -> IO ()) -> IO (Either String String)
runP4Env _ [] _ = return $ Left "p4h\n"
runP4Env env (subcmd : args) prepare = withP4Env env $ \p4 ->
  withCAString subcmd $ \cmd ->
    bracket (mapM newCString args) (mapM_ free) $ \argv' ->
      withArrayLen argv' $ \argc argv -> do
        unless (argc == 0) $ setArgv p4 (fromIntegral argc) argv
        prepare p4
        run p4 cmd
        getOutput2 p4

newP4 :: IO P4
newP4 = do
  (ptrUi, ptrClient) <- withPtrs_ $ \(ui, client) ->
    [C.block| void {
          *$(HsClientUser **ui) = new HsClientUser();
          *$(HsClientApi **client) = new HsClientApi();
          }
    |]
  fpClient <- newForeignPtr [C.funPtr| void free(HsClientApi *p) { delete p; } |] ptrClient
  fpUi <- newForeignPtr [C.funPtr| void free(HsClientUser *p) { delete p; } |] ptrUi
  return $ P4 fpClient fpUi

setHandler :: P4 -> P4Handler -> IO ()
setHandler (P4 _ fpUi) handler = do
  let out = get handler
  withCAString (show handler) $ \name ->
    [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->SetHandler($(const char *name), $fun-alloc:(void (*out)(const char *))); } |]
  where
    get (OutputBinary h) = packCString >=> h
    get (OutputInfo h) = peekCAString >=> h
    get (OutputText h) = peekCAString >=> h
    get (OutputMessage h) = peekCAString >=> h
    get (OutputStat h) = peekCAString >=> h

setPort :: P4 -> String -> IO ()
setPort (P4 fpClient _) port' =
  withCAString port' $ \port ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetPort($(const char *port)); } |]

setUser :: P4 -> String -> IO ()
setUser (P4 fpClient _) user' =
  withCAString user' $ \user ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetUser($(const char *user)); } |]

setPassword :: P4 -> String -> IO ()
setPassword (P4 fpClient _) password' =
  withCAString password' $ \password ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetPassword($(const char *password)); } |]

setHost :: P4 -> String -> IO ()
setHost (P4 fpClient _) host' =
  withCAString host' $ \host ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetHost($(const char *host)); } |]

setClient :: P4 -> String -> IO ()
setClient (P4 fpClient _) client' =
  withCAString client' $ \client ->
    [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetClient($(const char *client)); } |]

setInput :: P4 -> String -> IO ()
setInput (P4 _ fpUi) = flip withCAString $ \inp -> [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->SetInput($(const char *inp)); } |]

setArgv :: P4 -> C.CInt -> Ptr (Ptr C.CChar) -> IO ()
setArgv (P4 fpClient _) argc argv =
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetArgv($(int argc), $(char *const *argv)); } |]

init :: P4 -> IO ()
init (P4 fpClient fpUi) =
  [C.block| void {
      Error e;
      $fptr-ptr:(HsClientApi *fpClient)->Init(&e);
      if(e.Test())
        $fptr-ptr:(HsClientUser *fpUi)->HandleError(&e);
    }
  |]

run ::
  P4 ->
  Ptr C.CChar ->
  IO ()
run (P4 fpClient fpUi) cmd =
  [C.block| void {
      HsClientUser *ui = $fptr-ptr:(HsClientUser *fpUi);
      HsClientApi *client = $fptr-ptr:(HsClientApi *fpClient);
      client->Run( $(const char *cmd), ui );
      Error e;
      client->Final(&e);
      if (e.Test())
        ui->HandleError(&e);
    }
  |]

getOutput2 :: P4 -> IO (Either String String)
getOutput2 (P4 _ fpUi) = bracket getPtrs freePtrs $ \(msg, err) -> do
  guard (msg /= nullPtr && err /= nullPtr)
  liftM2 ret (peekCString msg) (peekCString err)
  where
    getPtrs = withPtrs_ $ \(msg, err) ->
      [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->GetOutput2($(const char **msg), $(const char **err)); } |]
    freePtrs (p1, p2) = free p1 >> free p2
    ret a "" = Right a
    ret _ b = Left b
