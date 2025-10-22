{-# LANGUAGE DeriveGeneric #-}

-- | Intermediate Representation for Agda's types
module Agda.IR where

import Data.Aeson
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | IR for IOCTM
data Response
  = -- raw form
    ResponseJSONRaw Value
  | ResponseEnd
  deriving (Generic)

instance ToJSON Response
