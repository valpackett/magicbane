{-# LANGUAGE Trustworthy, UnicodeSyntax #-}

-- | A Dropwizard-inspired web framework that integrates
--   Servant, monad-metrics/EKG, monad-logger/fast-logger, and other useful libraries
--   to provide a smooth web service development experience.
--
--   This module provides all the stuff you need for an application:
--   - reexports from all the modules below, including orphan instances from Magicbane.Has
--   - orphan instances for RIO
--   - reexports from various utility packages
--   - a basic example context for simple apps
module Magicbane (
  module X
, module Magicbane
) where

import           RIO.Orphans as X ()
import           Control.Error.Util as X hiding ((??), err, errLn, tryIO, handleExceptT, syncIO, bool)
import           Control.Monad.Trans.Maybe as X hiding (liftListen, liftPass, liftCallCC)
import           UnliftIO.Exception as X hiding (Handler)
import           UnliftIO.Concurrent as X
import           Data.List.Split as X (splitOn)
import           Data.String.Conversions as X hiding ((<>))
import           Data.String.Conversions.Monomorphic as X hiding (fromString)
import           Data.Aeson as X
import           Data.Aeson.QQ as X
import           Text.RawString.QQ as X
import           Network.URI as X
import           Network.HTTP.Link as X hiding (Link)
import           Network.HTTP.Types as X hiding (Header)
import           Network.Wai as X (Application, Middleware)
import           Network.Wai.Cli as X
import           Magicbane.App as X hiding (Or)
import           Magicbane.Config as X
import           Magicbane.Has as X
import           Magicbane.Logging as X
import           Magicbane.Metrics as X
import           Magicbane.Validation as X
import           Magicbane.HTTPClient as X
import           Magicbane.Util as X

type BasicContext = (ModHttpClient, ModLogger)
type BasicApp α = RIO BasicContext α

newBasicContext ∷ IO BasicContext
newBasicContext = do
  http ← newHttpClient
  (_, logg) ← newLogger (LogStdout defaultBufSize) simpleFormatter
  return (http, logg)
