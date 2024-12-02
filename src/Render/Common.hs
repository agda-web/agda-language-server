{-# LANGUAGE CPP #-}

module Render.Common where

import Agda.Syntax.Common
    ( Named(namedThing),
      Hiding(NotHidden, Hidden, Instance),
      LensHiding(getHiding),
      RewriteEqn'(..),
      MetaId(MetaId),
      LensQuantity(getQuantity),
      Quantity(..),
      LensRelevance(getRelevance),
      Relevance(..),
      Induction(..),
      Cohesion(..),
      QωOrigin(..),
      LensCohesion(getCohesion),
      NameId(..),
      Erased(..), asQuantity, Lock(..), LockOrigin (..),
    )
import qualified Agda.Utils.Null as Agda
import           Agda.Utils.List1 (toList)
import           Agda.Utils.Functor ((<&>))

import Render.Class
import Render.RichText

--------------------------------------------------------------------------------

-- | NameId
instance Render NameId where
  render (NameId n m) = text $ show n ++ "@" ++ show m

-- | MetaId
instance Render MetaId where
#if MIN_VERSION_Agda(2,6,3)
  render (MetaId n m) = text $ "_" ++ show n ++ "@" ++ show m
#else
  render (MetaId n) = text $ "_" ++ show n
#endif

-- | Relevance
instance Render Relevance where
  render Relevant = mempty
  render Irrelevant = "."
  render NonStrict = ".."

-- | Quantity
instance Render Quantity where
  render = \case
    Quantity0 o ->
      let s = show o
       in if Agda.null o
            then "@0"
            else text s
    Quantity1 o ->
      let s = show o
       in if Agda.null o
            then "@1"
            else text s
    Quantityω o -> render o

instance Render QωOrigin where
  render = \case
    QωInferred -> mempty
    Qω{}       -> "@ω"
    QωPlenty{} -> "@plenty"

instance Render Cohesion where
  render Flat   = "@♭"
  render Continuous = mempty
  render Squash  = "@⊤"

--------------------------------------------------------------------------------

-- | From 'prettyHiding'
--   @renderHiding info visible text@ puts the correct braces
--   around @text@ according to info @info@ and returns
--   @visible text@ if the we deal with a visible thing.
renderHiding :: LensHiding a => a -> (Inlines -> Inlines) -> Inlines -> Inlines
renderHiding a parensF =
  case getHiding a of
    Hidden -> braces'
    Instance {} -> dbraces
    NotHidden -> parensF

renderRelevance :: LensRelevance a => a -> Inlines -> Inlines
renderRelevance a d =
  if show d == "_" then d else render (getRelevance a) <> d

renderQuantity :: LensQuantity a => a -> Inlines -> Inlines
renderQuantity a d =
  if show d == "_" then d else render (getQuantity a) <+> d

instance Render Lock where
  render = \case
    IsLock LockOLock -> "@lock"
    IsLock LockOTick -> "@tick"
    IsNotLock -> mempty

#if MIN_VERSION_Agda(2,7,0)
renderErased :: Erased -> Inlines -> Inlines
renderErased = renderQuantity . asQuantity
#endif

renderCohesion :: LensCohesion a => a -> Inlines -> Inlines
renderCohesion a d =
  if show d == "_" then d else render (getCohesion a) <+> d

--------------------------------------------------------------------------------


instance (Render p, Render e) => Render (RewriteEqn' qn nm p e) where
  render = \case
    Rewrite es   -> prefixedThings (text "rewrite") (render . snd <$> toList es)
    Invert _ pes -> prefixedThings (text "invert") (toList pes <&> (\ (p, e) -> render p <+> "<-" <+> render e) . namedThing)

prefixedThings :: Inlines -> [Inlines] -> Inlines
prefixedThings kw = \case
  [] -> mempty
  (doc : docs) -> fsep $ (kw <+> doc) : fmap ("|" <+>) docs

instance Render Induction where
  render Inductive   = "inductive"
  render CoInductive = "coinductive"
