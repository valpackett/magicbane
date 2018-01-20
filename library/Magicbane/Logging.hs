{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- | Provides logging via monad-logger/fast-logger in a Magicbane app context.
module Magicbane.Logging (
  module Magicbane.Logging
, module X
) where

import           Data.Has
import           Control.Monad.Logger as X
import           System.Log.FastLogger
import           System.Log.FastLogger as X (LogType(..), defaultBufSize)
-- replacing ClassyPrelude
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid

newtype ModLogger = ModLogger (Loc → LogSource → LogLevel → LogStr → IO ())

instance (Has ModLogger α, Monad μ, MonadIO μ, MonadReader α μ) ⇒ MonadLogger μ where
  monadLoggerLog loc src lvl msg = asks getter >>= \(ModLogger f) → liftIO (f loc src lvl $ toLogStr msg)

instance (Has ModLogger α, MonadIO μ, MonadReader α μ) ⇒ MonadLoggerIO μ where
  askLoggerIO = (\(ModLogger f) → f) <$> asks getter

-- | Creates a logger module. Also returns the logger itself for using outside of your Magicbane app (e.g. in some WAI middleware).
newLogger ∷ LogType → IO (TimedFastLogger, ModLogger)
newLogger logtype = do
  tc ← newTimeCache simpleTimeFormat'
  (fl, _) ← newTimedFastLogger tc logtype
  -- forget cleanup because the logger will exist for the lifetime of the (OS) process
  return (fl, ModLogger $ \loc src lvl msg → fl (\t → toLogStr (t <> " ") <> defaultLogStr loc src lvl msg))
