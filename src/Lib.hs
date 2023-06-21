{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib (run) where

import Control.Monad (guard, liftM2)
import Foreign.C.String (newCString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import qualified Language.C.Inline.Cpp as C

C.context (C.cppCtx <> C.baseCtx)
C.include "<iostream>"
C.include "p4/clientapi.h"
C.include "p4/p4libs.h"
C.include "p4/hsclientuser.h"
C.using "namespace std"

run :: [String] -> IO (Either String String)
run [] = return $ Left "p4h\n"
run (subcmd : args') = do
  args <- mapM newCString args'
  alloca $ \p1 ->
    alloca $ \p2 ->
      withCString subcmd $ \c ->
        withArray args $ \argv -> do
          let argc = fromIntegral (length args')
          runP4 c argc argv p1 p2
          p1' <- peek p1
          p2' <- peek p2
          guard (p1' /= nullPtr && p2' /= nullPtr)
          val <- liftM2 ret (peekCString p1') (peekCString p2')
          free p1'
          free p2'
          mapM_ free args
          return val
  where
    ret a "" = Right a
    ret _ b = Left b

runP4 ::
  Ptr C.CChar ->
  C.CInt ->
  Ptr (Ptr C.CChar) ->
  Ptr (Ptr C.CChar) ->
  Ptr (Ptr C.CChar) ->
  IO ()
runP4 cmd argc argv msg err =
  [C.block| void {
      HsClientUser ui;
      ClientApi client;
      StrBuf msg;
      Error e;
      P4Libraries::Initialize( P4LIBRARIES_INIT_ALL, &e );
      if( e.Test() ) {
        e.Fmt( &msg );
        cerr << msg.Text() << endl;
        return;
      }
      client.SetPort("tcp:1666");
      client.Init(&e);
      if( e.Test() ) {
        e.Fmt( &msg );
        cerr << msg.Text() << endl;
        return;
      }
      if ($(int argc) > 0)
        client.SetArgv($(int argc), $(char **argv));
      client.Run( $(const char *cmd), &ui );
      client.Final( &e );
      ui.GetOutput2( $(const char **msg), $(const char **err));
      return;
    }
  |]
