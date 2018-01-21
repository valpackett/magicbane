{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

-- | 'Control.Monad.Except.ExceptT'-friendly concurrency
--   The selection of exposed functions matches "ClassyPrelude", but uses
--   "Control.Concurrent.Async.Lifted" instead of the 'Control.Monad.Except.ExceptT'-incompatible
--   "Control.Concurrent.Async.Lifted.Safe".
module Magicbane.Async (
  module Magicbane.Async
, module X
) where

import           Control.Concurrent.Lifted as X hiding (yield, throwTo)
import qualified Control.Concurrent.Lifted as Conc (yield)
import           Control.Concurrent.Async as X (Async, waitSTM, pollSTM, waitCatchSTM)
import           Control.Concurrent.Async.Lifted as X (async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask, withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask, withAsyncOnWithUnmask, asyncThreadId, race, race_, concurrently, mapConcurrently, Concurrently (..))
import           Control.Monad.Base

-- | Originally 'Conc.yield'. Renamed to avoid conflict with streaming libraries.
yieldThread :: MonadBase IO m => m ()
yieldThread = Conc.yield
{-# INLINE yieldThread #-}