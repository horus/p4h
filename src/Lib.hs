{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib (run) where

import Control.Monad (guard, liftM2)
import Foreign.C.String (newCString, peekCString, withCAString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline.Cpp (withPtrs_)
import qualified Language.C.Inline.Cpp as C

data ClientApi

data ClientUser

data P4 = P4 (ForeignPtr ClientApi) (ForeignPtr ClientUser)

C.context (C.cppCtx <> C.baseCtx <> C.fptrCtx <> C.cppTypePairs [("ClientApi", [t|ClientApi|]), ("HsClientUser", [t|ClientUser|])])
C.include "<iostream>"
C.include "p4/clientapi.h"
C.include "p4/hsclientuser.h"

run :: [String] -> IO (Either String String)
run [] = return $ Left "p4h\n"
run (subcmd : args') = do
  p4 <- newP4
  setPort p4 "tcp:1666"
  init' p4
  args <- mapM newCString args'
  withCAString subcmd $ \c ->
    withArray args $ \argv -> do
      let argc = fromIntegral (length args')
      run' p4 c argc argv
      mapM_ free args
      getOutput2 p4 ret
  where
    ret a "" = Right a
    ret _ b = Left b

newP4 :: IO P4
newP4 = do
  (ptrUi, ptrClient) <- withPtrs_ $ \(ui, client) ->
    [C.block| void {
          *$(HsClientUser **ui) = new HsClientUser();
          *$(ClientApi **client) = new ClientApi();
          }
    |]
  fpClient <- newForeignPtr [C.funPtr| void free(ClientApi *p) { delete p; } |] ptrClient
  fpUi <- newForeignPtr [C.funPtr| void free(HsClientUser *p) { delete p; } |] ptrUi
  return $ P4 fpClient fpUi

setPort :: P4 -> String -> IO ()
setPort (P4 fpClient _) port' =
  withCAString port' $ \port ->
    [C.block| void { $fptr-ptr:(ClientApi *fpClient)->SetPort($(char *port)); } |]

setInput :: P4 -> String -> IO ()
setInput (P4 _ fpUi) = flip withCAString $ \inp -> [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->SetInput($(char *inp)); } |]

init':: P4 -> IO ()
init' (P4 fpClient fpUi) =
  [C.block| void {
      Error e;
      $fptr-ptr:(ClientApi *fpClient)->Init(&e);
      if(e.Test())
        $fptr-ptr:(HsClientUser *fpUi)->HandleError(&e);
    }
  |]

run' ::
  P4 ->
  Ptr C.CChar ->
  C.CInt ->
  Ptr (Ptr C.CChar) ->
  IO ()
run' (P4 fpClient fpUi) cmd argc argv =
  [C.block| void {
      HsClientUser *ui = $fptr-ptr:(HsClientUser *fpUi);
      ClientApi *client = $fptr-ptr:(ClientApi *fpClient);
      if ($(int argc) > 0)
        client->SetArgv($(int argc), $(char **argv));
      client->Run( $(const char *cmd), ui );
      Error e;
      client->Final(&e);
      if (e.Test())
        ui->HandleError(&e);
    }
  |]

getOutput2 :: P4 -> (String -> String -> Either String String) -> IO (Either String String)
getOutput2 (P4 _ fpUi) ret = do
  (msg', err') <- withPtrs_ $ \(msg, err) ->
    [C.block| void { $fptr-ptr:(HsClientUser *fpUi)->GetOutput2( $(const char **msg), $(const char **err) ); } |]
  guard (msg' /= nullPtr && err' /= nullPtr)
  val <- liftM2 ret (peekCString msg') (peekCString err')
  free err' >> free msg'
  return val
