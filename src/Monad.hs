{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Monad where

import Agda.IR
import Agda.Interaction.Base (IOTCM, CurrentFile (..))
import Agda.TypeChecking.Monad (TCMT, SourceFile (SourceFile))
import Control.Concurrent
import Control.Monad.Reader
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Maybe (isJust)
import Data.Text
  ( Text,
    pack,
  )
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
  ( MonadLsp,
    getConfig,
  )
import Options
import Server.CommandController (CommandController)
import qualified Server.CommandController as CommandController
import Server.ResponseController (ResponseController)
import qualified Server.ResponseController as ResponseController
import Control.Monad.State (StateT (runStateT), mapStateT, MonadState (get))
import qualified Language.LSP.Server as LSP
import Agda.Utils.Time (getClockTime)
import Agda.Utils.FileName
import Agda.Syntax.Common (TopLevelModuleName'(..), ModuleNameHash (..))
import GHC.Exts
import Data.Word (Word64)

--------------------------------------------------------------------------------

data Env = Env
  { envOptions :: Options,
    envDevMode :: Bool,
    envConfig :: Config,
    envLogChan :: Chan Text,
    envCommandController :: CommandController,
    envResponseChan :: Chan Response,
    envResponseController :: ResponseController
  }

data Sta = Sta
  {
    staCurrentlyLoadedFile :: Maybe CurrentFile
  }

createInitEnv :: (MonadIO m, MonadLsp Config m) => Options -> m Env
createInitEnv options =
  Env options True
    <$> getConfig
    <*> liftIO newChan
    <*> liftIO CommandController.new
    <*> liftIO newChan
    <*> liftIO ResponseController.new

initSta :: IO Sta
initSta = do
  ts <- getClockTime
  return $ Sta { staCurrentlyLoadedFile = Just CurrentFile {
    currentFileStamp = ts,
    currentFileModule = TopLevelModuleName {
      moduleNameId = ModuleNameHash { moduleNameHash = (123 :: Word64) },
      moduleNameInferred = False
    },
    currentFileArgs = [],
    currentFilePath = AbsolutePath "XXX" } }

--------------------------------------------------------------------------------

-- | OUR monad
type ServerM m = StateT Sta (ReaderT Env m)

runServerM :: Env -> Sta -> ServerM m a -> m (a, Sta)
runServerM e st m = runReaderT (runStateT m st) e

liftServer :: Monad m => m a -> ServerM m a
liftServer = lift <$> lift

mapServerT :: (Monad m, Monad n) =>
  (m (a, Sta) -> n (b, Sta)) -> ServerM m a -> ServerM n b
mapServerT = mapStateT . mapReaderT

--------------------------------------------------------------------------------

writeLog :: (Monad m, MonadIO m) => Text -> ServerM m ()
writeLog msg = do
  chan <- asks envLogChan
  liftIO $ writeChan chan msg

writeLog' :: (Monad m, MonadIO m, Show a) => a -> ServerM m ()
writeLog' x = do
  chan <- asks envLogChan
  liftIO $ writeChan chan $ pack $ show x

-- | Provider
provideCommand :: (Monad m, MonadIO m) => IOTCM -> ServerM m ()
provideCommand iotcm = do
  controller <- asks envCommandController
  liftIO $ CommandController.put controller iotcm

-- | Consumter
consumeCommand :: (Monad m, MonadIO m) => Env -> m IOTCM
consumeCommand env = liftIO $ CommandController.take (envCommandController env)

waitUntilResponsesSent :: (Monad m, MonadIO m) => ServerM m ()
waitUntilResponsesSent = do
  controller <- asks envResponseController
  liftIO $ ResponseController.setCheckpointAndWait controller

signalCommandFinish :: (Monad m, MonadIO m) => ServerM m ()
signalCommandFinish = do
  writeLog "[Command] Finished"
  -- send `ResponseEnd`
  env <- ask
  liftIO $ writeChan (envResponseChan env) ResponseEnd
  -- allow the next Command to be consumed
  liftIO $ CommandController.release (envCommandController env)

-- | Sends a Response to the client via "envResponseChan"
sendResponse :: (Monad m, MonadIO m) => Env -> Response -> TCMT m ()
sendResponse env response = liftIO $ writeChan (envResponseChan env) response
