{-# LANGUAGE CPP #-}

module Server.Handler where

import           Agda                           ( getCommandLineOptions
                                                , runAgda
                                                )
import qualified Agda.IR                       as IR

import           Agda.Interaction.Base          ( CommandQueue(..)
#if MIN_VERSION_Agda(2,7,0)
#else
                                                , CommandM
#endif
                                                , CommandState(optionsOnReload)
                                                , Rewrite(AsIs)
                                                , initCommandState, CurrentFile (currentFilePath)
                                                )
import           Agda.Interaction.BasicOps      ( atTopLevel
                                                , typeInCurrent
                                                )
import           Agda.Interaction.Highlighting.Precise
                                                ( HighlightingInfo )
import qualified Agda.Interaction.Imports      as Imp
import           Agda.Interaction.InteractionTop
                                                ( cmd_load'
#if MIN_VERSION_Agda(2,8,0)
#else
                                                , localStateCommandM
#endif

#if MIN_VERSION_Agda(2,8,0)
#elif MIN_VERSION_Agda(2,7,0)
                                                , CommandM
#else
#endif
                                                )
import           Agda.Interaction.Options       ( CommandLineOptions
                                                  ( optAbsoluteIncludePaths
                                                  )
                                                )
import qualified Agda.Parser                   as Parser
import           Agda.Position                  ( makeToOffset
                                                , toAgdaPositionWithoutFileLC
                                                )
import           Agda.Syntax.Abstract.Pretty    ( prettyATop )
import           Agda.Syntax.Parser             ( exprParser
                                                , parse
                                                )
import           Agda.Syntax.Position           (getRange
                                                , Range' (Range)
                                                , getRangeWithoutFile
                                                , Position' (..)
                                                , iStart'
                                                , iEnd'
                                                )
import           Agda.Syntax.Translation.ConcreteToAbstract
                                                ( concreteToAbstract_ )
import           Agda.TypeChecking.Monad        ( HasOptions(commandLineOptions)
                                                , setInteractionOutputCallback, putTC, lensPersistentState, TCState (stPersistentState), getTC, SessionTCState (..), askTC
                                                )
#if MIN_VERSION_Agda(2,8,0)
import           Agda.Interaction.Command       ( CommandM, localStateCommandM )
import           Agda.TypeChecking.Monad.Trace  ( runPM )
#endif
#if MIN_VERSION_Agda(2,8,0)
#else
import           Agda.TypeChecking.Warnings     ( runPM )
#endif
import           Agda.Syntax.Common.Pretty      ( render, prettyShow )
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe      (hoistMaybe
                                                , runMaybeT
                                                , MaybeT (MaybeT)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq((:<|), Empty) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( LspM )
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import qualified Language.LSP.Protocol.Lens    as VFS
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config
                                                , Options(optRawAgdaOptions)
                                                )

import Agda.Syntax.Parser.Tokens (Token(..), Symbol (..))
import Data.IORef (readIORef)
import Agda.Utils.FileName (absolute)

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: CommandM a -> ServerM (LspM Config) (Either String a)
runCommandM program = do
  env <- ask
  runAgda $ do
    -- restore tc state from agda session
    m <- liftIO $ readIORef $ envTCState env
    _ <- case m of
      Just tcSt -> putTC tcSt
      Nothing -> return ()

    -- get command line options
    options <- getCommandLineOptions

    -- we need to set InteractionOutputCallback else it would panic
    lift $ setInteractionOutputCallback $ \_response -> return ()

    -- setup the command state
    commandQueue <- liftIO initialiseCommandQueue
    let commandState = (initCommandState commandQueue)
          { optionsOnReload = options { optAbsoluteIncludePaths = [] }
          }

    lift $ evalStateT program commandState

inferTypeOfText
  :: FilePath -> Text -> ServerM (LspM Config) (Either String String)
inferTypeOfText filepath text = do
  env <- ask
  curFile <- liftIO $ readIORef $ envCurrentFile env
  fpath <- liftIO $ absolute filepath

  -- bail out if the file is not loaded yet; loading from scratch might be very slow
  -- the downside is that the hover is nearly unusable if the user frequently switch between files
  case curFile of
    Nothing -> return $ Left "File is not loaded yet."
    Just f -> do
      let afpath = currentFilePath f
      if afpath /= fpath then
        return $ Left "File is not active. Load it and try again."
      else go

  where
    go = runCommandM $ do
      -- load first
      -- cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
      -- infer later
      let norm = AsIs
      -- localStateCommandM: restore TC state afterwards, do we need this here?
      typ <- localStateCommandM $ do
        (e, _attrs) <- lift $ runPM $ parse exprParser (unpack text)
        lift $ atTopLevel $ do
          concreteToAbstract_ e >>= typeInCurrent norm

      render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> ServerM (LspM Config) (LSP.Hover LSP.|? LSP.Null)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  output <- runMaybeT $ do
    file <- hoistMaybe result

    let source      = VFS.virtualFileText file
    VFS.CodePointPosition line col <- hoistMaybe $
      VFS.positionToCodePointPosition file pos
    let rope = VFS._file_text file
    let agdaPos = toAgdaPositionWithoutFileLC rope line col
    (token, text) <- MaybeT $ Parser.tokenAt uri source agdaPos

    -- filter out uninteresting tokens
    _ <- hoistMaybe $ case token of
        TokId _                     -> Just ()
        TokQId _                    -> Just ()
        TokLiteral _                -> Just ()
        TokSymbol SymQuestionMark _ -> Just ()
        TokString _                 -> Just ()
        _                           -> Nothing

    let Range () intvs = getRangeWithoutFile token

    intv <- hoistMaybe $ case intvs of
      x :<| _ -> Just x
      _ -> Nothing

    let
      Pn () _ l0 c0 = iStart' intv
      Pn () _ l1 c1 = iEnd' intv
      cprange = VFS.CodePointRange
        (VFS.CodePointPosition (fromIntegral l0 - 1) (fromIntegral c0 - 1))
        (VFS.CodePointPosition (fromIntegral l1 - 1) (fromIntegral c1 - 1))

    range <- hoistMaybe $ VFS.codePointRangeToRange file cprange

    filepath <- hoistMaybe $ LSP.uriToFilePath uri
    inferResult <- lift $ inferTypeOfText filepath text
    case inferResult of
      Left err -> do
        let content = hoverContent $ "Error: " <> pack err
        return $ LSP.InL $ LSP.Hover content (Just range)
      Right typeString -> do
        let content = hoverContent $ pack typeString
        return $ LSP.InL $ LSP.Hover content (Just range)

  return $ fromMaybe (LSP.InR LSP.Null) output

  where
      hoverContent =
        LSP.InL . LSP.mkMarkdownCodeBlock "agda-language-server"
--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute


fromHighlightingInfo :: IR.HighlightingInfo -> LSP.SemanticTokenAbsolute
fromHighlightingInfo (IR.HighlightingInfo start end aspects isTokenBased note defSrc)
  = LSP.SemanticTokenAbsolute 1 1 3 kw []
  where
    kw = LSP.SemanticTokenTypes_Keyword

-- HighlightingInfo
--       Int -- starting offset
--       Int -- ending offset
--       [String] -- list of names of aspects
--       Bool -- is token based?
--       String -- note
--       (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module

-- toToken
--   :: Ranged a
--   => J.SemanticTokenTypes
--   -> [J.SemanticTokenModifiers]
--   -> a
--   -> [J.SemanticTokenAbsolute]
-- toToken types modifiers x =
--   let range = rangeOf x
--   in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
--                                 (posCol (rangeStart range) - 1)
--                                 (rangeSpan range)
--                                 types
--                                 modifiers
--       ]
