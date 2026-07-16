{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (when)
import Options
import Server (run)
-- import Simple (run)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.Environment
import System.FilePath ((</>))
import System.IO
import Text.Read (readMaybe)

#if defined(REACTOR)
import GHC.Wasm.Prim
import Server (runFromReactor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
#endif

#if MIN_VERSION_Agda(2,8,0)
import Agda.Setup (setup)
#endif

main :: IO ()
main = do
  -- set locale to UTF-8
  -- https://github.com/agda/agda-language-server/issues/24
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetEncoding stderr utf8

-- getExecutablePath returns argv[0] in WASM, which is useless
#ifndef wasm32_HOST_ARCH
  -- The GitHub CI-built executable lacks the correct data directory path.
  -- If there's directory named "data" in the executable's directory,
  -- then we assume that the executable is built by GitHub CI
  -- and we should set the $Agda_datadir environment variable to the correct directory.
  executablePath <- getExecutablePath
  let dataDir = executablePath </> "data"
  isBuiltByCI <- doesDirectoryExist dataDir
  when isBuiltByCI $ do
    setEnv "Agda_datadir" dataDir
#endif

  options <- getOptionsFromArgv
  case () of
    _ | optHelp options -> putStrLn usageMessage
      | optVersion options -> putStrLn versionString
#if MIN_VERSION_Agda(2,8,0)
      | optSetup options -> do
          setup True
          return ()
#endif
      | otherwise -> do
          _ <- run options
          -- _ <- run
          return ()


#if defined(REACTOR)

data Env = Env
  { options :: Options
  , incomingMessage :: MVar B.StrictByteString
  , outgoingMessage :: MVar String
  }

initialEnv :: IO Env
initialEnv = Env <$> getOptionsFromArgv <*> newEmptyMVar <*> newEmptyMVar

type ServerHandle = StablePtr Env

foreign export javascript "run_setup"
  runSetup :: IO ()

foreign export javascript "new_language_server"
  newLanguageServer :: IO ServerHandle

foreign export javascript "run_language_server"
  runLanguageServer :: ServerHandle -> IO Int

foreign export javascript "free_language_server"
  freeLanguageServer :: ServerHandle -> IO ()

foreign export javascript "send_message"
  sendMessage :: ServerHandle -> JSString -> IO ()

foreign export javascript "recv_message"
  recvMessage :: ServerHandle -> IO JSString

runSetup :: IO ()
runSetup = setup True

newLanguageServer :: IO ServerHandle
newLanguageServer = initialEnv >>= newStablePtr

freeLanguageServer :: ServerHandle -> IO ()
freeLanguageServer = freeStablePtr

runLanguageServer :: ServerHandle -> IO Int
runLanguageServer hdl = do
  env <- deRefStablePtr hdl

  let
    serverInwards :: IO B.StrictByteString
    serverInwards = takeMVar (incomingMessage env)

    serverOutwards :: BL.LazyByteString -> IO ()
    serverOutwards s = (return . TL.unpack . decodeUtf8) s >>= putMVar (outgoingMessage env)

  runFromReactor serverInwards serverOutwards (options env)


sendMessage :: ServerHandle -> JSString -> IO ()
sendMessage hdl s = do
  env <- deRefStablePtr hdl
  let input = fromJSString s
  putMVar (incomingMessage env) $ (encodeUtf8 . T.pack) input
  -- put an EOF to signal message end
  putMVar (incomingMessage env) $ ""
  return ()

recvMessage :: ServerHandle -> IO JSString
recvMessage hdl = do
  env <- deRefStablePtr hdl
  str <- takeMVar (outgoingMessage env)
  return $ toJSString str

#else

data JSString = JSString {}

fromJSString :: JSString -> String
fromJSString = undefined
toJSString :: String -> JSString
toJSString = undefined

#endif
