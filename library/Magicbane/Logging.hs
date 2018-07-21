{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- | Provides logging via fast-logger in a Magicbane app context.
module Magicbane.Logging (
  module Magicbane.Logging
, module X
) where

import           RIO
import           Data.Has
import           System.Log.FastLogger
import           System.Log.FastLogger as X (LogType(..), defaultBufSize)

type ModLogger = LogFunc

instance {-# OVERLAPPABLE #-} Has ModLogger α ⇒ HasLogFunc α where
  logFuncL = hasLens

type Formatter = TimedFastLogger → CallStack → LogSource → LogLevel → Utf8Builder → IO ()

-- | Creates a logger module using a given formatting function.
-- | Also returns the underlying TimedFastLogger for use outside of your Magicbane app (e.g. in some WAI middleware).
newLogger ∷ LogType → Formatter → IO (TimedFastLogger, ModLogger)
newLogger logtype formatter = do
  tc ← newTimeCache simpleTimeFormat'
  (fl, _) ← newTimedFastLogger tc logtype
  -- forget cleanup because the logger will exist for the lifetime of the (OS) process
  return (fl, mkLogFunc $ formatter fl)

simpleFormatter ∷ Formatter
simpleFormatter logger cs src level msg =
  logger $ \t →
    toLogStr t <> " " <>
    toLogStr (utf8BuilderToText $ displayCallStack cs) <> " " <>
    toLogStr src <> " " <>
    toLogStr (showLevel level) <>
    toLogStr (utf8BuilderToText msg) <> "\n"
  where showLevel LevelDebug     = "[DEBUG] "
        showLevel LevelInfo      = "[ INFO] "
        showLevel LevelWarn      = "[ WARN] "
        showLevel LevelError     = "[ERROR] "
        showLevel (LevelOther t) = "[" <> t <> "] "
