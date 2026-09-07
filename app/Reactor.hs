{-# LANGUAGE CPP #-}
module Reactor where

import GHC.Wasm.Prim
import Options
import Server (serverDefn)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Language.LSP.Server hiding (Options)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import qualified Data.Attoparsec.ByteString as Attoparsec

#if MIN_VERSION_Agda(2,8,0)
import Agda.Setup (setup)
#endif

import Agda (parseToplevelModuleName)
import Agda.TypeChecking.Monad (runTCMTop)

data ReactorEnv = ReactorEnv
  { rOptions :: Options
  , rIncomingMessage :: MVar B.StrictByteString
  , rOutgoingMessage :: MVar String
  }

initialEnv :: IO ReactorEnv
initialEnv = ReactorEnv <$> getOptionsFromArgv <*> newEmptyMVar <*> newEmptyMVar

type ServerHandle = StablePtr ReactorEnv

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

foreign export javascript "parse_module_name sync"
  parseModuleName :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "return new Error($1)"
  js_new_error :: JSString -> IO JSVal

foreign import javascript unsafe "return []"
  js_new_array :: IO JSVal

foreign import javascript unsafe "$1.push($2); return $1"
  js_array_push :: JSVal -> JSVal -> IO JSVal

runSetup :: IO ()
#if MIN_VERSION_Agda(2,8,0)
runSetup = setup True
#else
runSetup = error "This Agda version does not have setup functionality."
#endif

newLanguageServer :: IO ServerHandle
newLanguageServer = initialEnv >>= newStablePtr

freeLanguageServer :: ServerHandle -> IO ()
freeLanguageServer = freeStablePtr

runLanguageServer :: ServerHandle -> IO Int
runLanguageServer hdl = do
  env <- deRefStablePtr hdl

  let
    serverInwards :: IO B.StrictByteString
    serverInwards = takeMVar (rIncomingMessage env)

    serverOutwards :: BL.LazyByteString -> IO ()
    serverOutwards s = (return . TL.unpack . decodeUtf8) s >>= putMVar (rOutgoingMessage env)

  runFromReactor serverInwards serverOutwards (rOptions env)

runFromReactor :: IO B.StrictByteString -> (BL.LazyByteString -> IO ()) -> Options -> IO Int
runFromReactor serverInwards serverOutwards options = do
  runServerWithConfig serverConfig (serverDefn options)
  where
    serverConfig :: ServerConfig Config
    serverConfig = ServerConfig
      { ioLogger = mempty
      , lspLogger = mempty
      , inwards = serverInwards
      , outwards = serverOutwards
      , prepareOutwards = id
      , parseInwards = do
          -- using takeByteString here will make it return partial result,
          -- requiring another empty string to signal its end
          chunk <- Attoparsec.getChunk
          case chunk of
            Nothing -> pure B.empty
            Just xs -> Attoparsec.take $ B.length xs
      }

sendMessage :: ServerHandle -> JSString -> IO ()
sendMessage hdl s = do
  env <- deRefStablePtr hdl
  let input = fromJSString s
  putMVar (rIncomingMessage env) $ (encodeUtf8 . T.pack) input
  return ()

recvMessage :: ServerHandle -> IO JSString
recvMessage hdl = do
  env <- deRefStablePtr hdl
  str <- takeMVar (rOutgoingMessage env)
  return $ toJSString str

toJSVal :: JSString -> JSVal
toJSVal (JSString val) = val

parseModuleName :: JSString -> JSString -> IO JSVal
parseModuleName fname src = do
  let fname' = T.pack . fromJSString $ fname
  let src' = T.pack . fromJSString $ src
  result <- runTCMTop $ parseToplevelModuleName fname' src'
  case result of
    Left err -> do
      js_new_error $ toJSString $ show err
    Right ts -> do
      arr <- js_new_array
      let xs = map (toJSVal . toJSString . T.unpack) ts
      sequence_ $ map (js_array_push arr) xs
      return arr

-- for shimming types when GHC.Wasm.Prim is not available, e.g., when using HLS
#if 0
data JSVal = JSVal {}
newtype JSString = JSString JSVal

fromJSString :: JSString -> String
fromJSString = undefined
toJSString :: String -> JSString
toJSString = undefined

js_new_error :: JSString -> IO JSVal
js_new_error = undefined
js_new_array :: IO JSVal
js_new_array = undefined
js_array_push :: JSVal -> JSVal -> IO JSVal
js_array_push = undefined
#endif
