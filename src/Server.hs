{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- entry point of the LSP server

module Server (
    run
  , serverDefn
) where

import qualified Agda
import Control.Concurrent (writeChan)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (liftIO), ask)
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.Aeson as JSON
import Data.Text (pack)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types (HoverParams (..), SaveOptions (..), TextDocumentIdentifier (..), TextDocumentSyncKind (..), TextDocumentSyncOptions (..), type (|?) (..))
import Language.LSP.Server hiding (Options)
import qualified Language.LSP.Server hiding (Options)
import qualified Language.LSP.Server as LSP
import Monad
import Options
import qualified Server.Handler as Handler
import Switchboard (Switchboard, agdaCustomMethod)
import qualified Switchboard

#if defined(wasm32_HOST_ARCH)
import Agda.Utils.IO (catchIO)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (stdInput, setFdOption, FdOption (..))
#else
import qualified Network.Simple.TCP as TCP
import Network.Socket (socketToHandle)
#endif

--------------------------------------------------------------------------------

run :: Options -> IO Int
run options = do
  case optViaTCP options of
    Just port -> do
#if defined(wasm32_HOST_ARCH)
      error "WASM does not support listening to a port."
#else
      void $
        TCP.serve (TCP.Host "127.0.0.1") (show port) $
          \(sock, _remoteAddr) -> do
            -- writeChan (envLogChan env) "[Server] connection established"
            handle <- socketToHandle sock ReadWriteMode
            _ <- runServerWithHandles mempty mempty handle handle (serverDefn options)
            return ()
      -- Switchboard.destroy switchboard
      return 0
#endif
    Nothing -> do
#if defined(wasm32_HOST_ARCH)
      liftIO $ setFdOption stdInput NonBlockingRead True
        `catchIO` (\ (e :: IOError) -> hPutStrLn stderr $ "Failed to enable nonblocking on stdin: " ++ (show e) ++ "\nThe WASM module might not behave correctly.")
#endif
      runServer (serverDefn options)

serverDefn :: Options -> ServerDefinition Config
serverDefn options =
  ServerDefinition
    { defaultConfig = initConfig,
      onConfigChange = const $ pure (),
      parseConfig = \old newRaw -> case JSON.fromJSON newRaw of
        JSON.Error s -> Left $ pack $ "Cannot parse server configuration: " <> s
        JSON.Success new -> Right new,
      doInitialize = \ctxEnv _req -> do
        env <- runLspT ctxEnv (createInitEnv options)
        switchboard <- Switchboard.new env
        Switchboard.setupLanguageContextEnv switchboard ctxEnv
        pure $ Right (ctxEnv, env),
      configSection = "dummy",
      staticHandlers = const $ submitToReactor handlers,
      interpretHandler = \(ctxEnv, env) ->
        Iso
          { forward = runLspT ctxEnv . runServerM env,
            backward = liftIO
          },
      options = lspOptions
    }

lspOptions :: LSP.Options
lspOptions = defaultOptions {optTextDocumentSync = Just syncOptions}

submitToReactor :: (m ~ ServerM (LspM Config)) => Handlers m -> Handlers m
submitToReactor = mapHandlers goReq goNoti
  where
    goReq :: forall (a :: Method ClientToServer Request). Handler (ServerM (LspM Config)) a -> Handler (ServerM (LspM Config)) a
    goReq f = \msg k -> do
      ctxEnv <- getLspEnv
      env <- ask
      liftIO $ writeChan (envLspRequestChan env) (ReactorInput $ runLspT ctxEnv . runServerM env $ f msg k)
    goNoti :: forall (a :: Method ClientToServer Notification). Handler (ServerM (LspM Config)) a -> Handler (ServerM (LspM Config)) a
    goNoti f = \msg -> do
      ctxEnv <- getLspEnv
      env <- ask
      liftIO $ writeChan (envLspRequestChan env) (ReactorInput $ runLspT ctxEnv . runServerM env $ f msg)

-- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
-- syncOptions :: TextDocumentSyncOptions
-- syncOptions =
--   TextDocumentSyncOptions
--     { _openClose = Just True, -- receive open and close notifications from the client
--       _change = Just changeOptions, -- receive change notifications from the client
--       _willSave = Just False, -- receive willSave notifications from the client
--       _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
--       _save = Just $ InR saveOptions
--     }
syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    { _openClose = Just True, -- receive open and close notifications from the client
      _change = Just TextDocumentSyncKind_Incremental, -- receive change notifications from the client
      _willSave = Just False, -- receive willSave notifications from the client
      _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
      _save = Just $ InR $ SaveOptions (Just True) -- includes the document content on save, so that we don't have to read it from the disk (not sure if this is still true in lsp 2)
    }

-- handlers of the LSP server
handlers :: Handlers (ServerM (LspM Config))
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler agdaCustomMethod $ \req responder -> do
        let TRequestMessage _ _i _ params = req
        response <- Agda.sendCommand params
        responder $ Right response,
      -- `textDocument/hover`
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
        result <- Handler.onHover uri pos
        responder $ Right result,
      -- -- syntax highlighting
      -- , requestHandler STextDocumentSemanticTokensFull $ \req responder -> do
      --   result <- Handler.onHighlight (req ^. (params . textDocument . uri))
      --   responder result

      -- `initialized`
      notificationHandler SMethod_Initialized $ \_notification -> return (),
      -- `workspace/didChangeConfiguration`
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification -> return (),
      -- `textDocument/didOpen`
      notificationHandler SMethod_TextDocumentDidOpen $ \_notification -> return (),
      -- `textDocument/didClose`
      notificationHandler SMethod_TextDocumentDidClose $ \_notification -> return (),
      -- `textDocument/didChange`
      notificationHandler SMethod_TextDocumentDidChange $ \_notification -> return (),
      -- `textDocument/didSave`
      notificationHandler SMethod_TextDocumentDidSave $ \_notification -> return ()
    ]
