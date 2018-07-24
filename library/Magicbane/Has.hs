{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, UnicodeSyntax, FlexibleInstances, UndecidableInstances #-}

-- | Orphan instances and utility functions for Data.Has, a typeclass for extracting values from a structure by type.
module Magicbane.Has (
  module X
, module Magicbane.Has
) where

import           Data.Has as X
import           Control.Monad.Reader (MonadReader, asks)
import           RIO (HasLogFunc(..))
import           Magicbane.Logging (ModLogger)
import           Magicbane.Metrics (ModMetrics(..), MonadMetrics, getMetrics)
import           Magicbane.HTTPClient (ModHttpClient(..), HasHttpManager, getHttpManager)

instance {-# OVERLAPPABLE #-} (Has ModHttpClient α) ⇒ HasHttpManager α where
  getHttpManager = (\(ModHttpClient m) → m) <$> getter

instance {-# OVERLAPPABLE #-} Has ModLogger α ⇒ HasLogFunc α where
  logFuncL = hasLens

instance (Has ModMetrics α, Monad μ, MonadReader α μ) ⇒ MonadMetrics μ where
  getMetrics = (\(ModMetrics m) → m) <$> asks getter

-- | Gets a value of any type from the context.
askObj ∷ (Has β α, MonadReader α μ) ⇒ μ β
askObj = asks getter

-- | Gets a thing from a value of any type from the context. (Useful for configuration fields.)
askOpt ∷ (Has β α, MonadReader α μ) ⇒ (β → ψ) → μ ψ
askOpt f = asks $ f . getter
