{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib where

import Control.Exception (bracket)
import Control.Monad (guard, liftM2, unless)
import Foreign.C.String (newCString, peekCString, withCAString)
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

C.context
  ( C.cppCtx
      <> C.baseCtx
      <> C.fptrCtx
      <> C.cppTypePairs
        [ ("HsClientApi", [t|ClientAPI|]),
          ("HsClientUser", [t|ClientUser|])
        ]
  )
C.include "<iostream>"
C.include "hsclientapi.h"
C.include "hsclientuser.h"

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

connect :: IO P4
connect = connectEnv def
  where
    def = P4Env Nothing Nothing Nothing Nothing Nothing Nothing

disconnect :: P4 -> IO ()
disconnect (P4 fpClient fpUi) =
  [C.block| void {
      Error e;
      $fptr-ptr:(HsClientApi *fpClient)->Final(&e);
      if (e.Test())
        $fptr-ptr:(HsClientUser *fpUi)->HandleError(&e);
    }
  |]

withP4 :: (P4 -> IO a) -> IO a
withP4 = bracket connect disconnect

runP4 :: [String] -> IO (Either String String)
runP4 [] = return $ Left "p4h\n"
runP4 (subcmd : args) = withP4 $ \p4 ->
  withCAString subcmd $ \cmd ->
    bracket (mapM newCString args) (mapM_ free) $ \argv' ->
      withArrayLen argv' $ \argc argv -> do
        unless (argc == 0) $ setArgv p4 (fromIntegral argc) argv
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
setInput (P4 _ fpUi) = flip withCAString $ \inp -> [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->SetInput($(char *inp)); } |]

setArgv :: P4 -> C.CInt -> Ptr (Ptr C.CChar) -> IO ()
setArgv (P4 fpClient _) argc argv =
  [C.block| void { $fptr-ptr:(HsClientApi *fpClient)->SetArgv($(int argc), $(char **argv)); } |]

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
      [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->GetOutput2( $(const char **msg), $(const char **err) ); } |]
    freePtrs (p1, p2) = free p1 >> free p2
    ret a "" = Right a
    ret _ b = Left b
