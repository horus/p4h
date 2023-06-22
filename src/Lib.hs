{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib (run) where

import Control.Monad (guard, liftM2)
import Foreign.C.String (newCString, peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Language.C.Inline.Cpp (withPtrs_)
import qualified Language.C.Inline.Cpp as C

data ClientApi

data ClientUser

data P4 = P4 (ForeignPtr ClientApi) (ForeignPtr ClientUser)

C.context (C.cppCtx <> C.baseCtx <> C.cppTypePairs [("ClientApi", [t|ClientApi|]), ("HsClientUser", [t|ClientUser|])])
C.include "<iostream>"
C.include "p4/clientapi.h"
C.include "p4/p4libs.h"
C.include "p4/hsclientuser.h"

run :: [String] -> IO (Either String String)
run [] = return $ Left "p4h\n"
run (subcmd : args') =
  initClient >>= \case
    Left err -> return $ Left err
    Right p4 -> do
      args <- mapM newCString args'
      withCString subcmd $ \c ->
        withArray args $ \argv -> do
          let argc = fromIntegral (length args')
          run' p4 c argc argv
          mapM_ free args
          getOutput2 p4 ret
  where
    ret a "" = Right a
    ret _ b = Left b

initClient :: IO (Either String P4)
initClient = do
  (ptrErr, ptrUi, ptrClient) <- withPtrs_ $ \(err, ui, client) ->
    [C.block| void {
            StrBuf msg;
            Error e;
            *$(char **err) = (char *)NULL;
            *$(HsClientUser **ui) = new HsClientUser();
            *$(ClientApi **client) = new ClientApi();
            P4Libraries::Initialize( P4LIBRARIES_INIT_ALL, &e );
            if( e.Test() ) {
              e.Fmt( &msg );
              *$(char **err) = strdup(msg.Text());
              return;
          }
          (*$(ClientApi **client))->SetPort("tcp:1666");
          (*$(ClientApi **client))->Init(&e);
          if( e.Test() ) {
            e.Fmt( &msg );
            *$(char **err) = strdup(msg.Text());
            return;
          }
        }
    |]
  if ptrErr == nullPtr
    then do
      fpClient <- newForeignPtr [C.funPtr| void free(ClientApi *p) { delete p; } |] ptrClient
      fpUi <- newForeignPtr [C.funPtr| void free(HsClientUser *p) { delete p; } |] ptrUi
      return $ Right $ P4 fpClient fpUi
    else do
      err' <- peekCString ptrErr
      [C.block| void {
          delete $(HsClientUser *ptrUi);
          delete $(ClientApi *ptrClient);
          free($(char *ptrErr));
        }
      |]
      return $ Left err'

run' ::
  P4 ->
  Ptr C.CChar ->
  C.CInt ->
  Ptr (Ptr C.CChar) ->
  IO ()
run' (P4 fpClient fpUi) cmd argc argv =
  withForeignPtr fpClient $ \ptrClient ->
    withForeignPtr fpUi $ \ptrUi ->
      [C.block| void {
          HsClientUser *ui = $(HsClientUser *ptrUi);
          ClientApi *client = $(ClientApi *ptrClient);
          StrBuf msg;
          Error e;
          if ($(int argc) > 0)
            client->SetArgv($(int argc), $(char **argv));
          client->Run( $(const char *cmd), ui );
          if ( client->Final( &e ) > 0 ) {
            if (e.Test()) {
              e.Fmt( &msg );
              ui->OutputError( msg.Text() );
            }
          }
        }
      |]

getOutput2 :: P4 -> (String -> String -> Either String String) -> IO (Either String String)
getOutput2 (P4 _ fpUi) ret = do
  (msg', err') <- withPtrs_ $ \(msg, err) ->
    withForeignPtr fpUi $ \ptrUi ->
      [C.block| void { $(HsClientUser *ptrUi)->GetOutput2( $(const char **msg), $(const char **err) ); } |]
  guard (msg' /= nullPtr && err' /= nullPtr)
  val <- liftM2 ret (peekCString msg') (peekCString err')
  free err' >> free msg'
  return val
