{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE Trustworthy, NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, LambdaCase #-}

-- | A Dropwizard-inspired web framework that integrates
--   Servant, monad-metrics/EKG, monad-logger/fast-logger, and other useful libraries
--   to provide a smooth web service development experience.
--
--   This module provides all the stuff you need.
module Magicbane (
  module X
, module Magicbane
) where

import           Control.Error.Util as X hiding (hoistEither, (??), tryIO, bool)
import           Control.Monad.Trans.Control as X
import           Control.Monad.Trans.Maybe as X hiding (liftListen, liftPass, liftCallCC)
import qualified System.Envy
import           System.Envy as X hiding ((.=), (.!=), decode)
import qualified System.IO
import           Data.Default as X
import           Data.List.Split as X (splitOn)
import           Data.String.Conversions as X hiding ((<>))
import           Data.String.Conversions.Monomorphic as X
import           Data.Aeson as X
import           Data.Aeson.QQ as X
import           Text.RawString.QQ as X
import           Network.URI as X
import qualified Network.HTTP.Link
import           Network.HTTP.Link as X hiding (Link)
import           Network.HTTP.Types as X hiding (Header)
import           Network.Wai as X (Application, Middleware)
import           Network.Wai.Cli as X hiding (port)
import           Magicbane.App as X hiding (Or)
import           Magicbane.Async as X
import           Magicbane.Logging as X
import           Magicbane.Metrics as X
import           Magicbane.Validation as X
import           Magicbane.HTTPClient as X
import           Magicbane.Util as X
-- replacing ClassyPrelude
import           Data.Text (Text)
import           Control.Monad.IO.Class
import           System.IO (stderr)

type Host = Header "Host" Text
type Form = ReqBody '[FormUrlEncoded] [(Text, Text)]

type HTTPLink = Network.HTTP.Link.Link
type WithLink α = (Headers '[Header "Link" [HTTPLink]] α)

instance (Default α) ⇒ DefConfig α where
  defConfig = def

decodeEnvy = System.Envy.decode

-- | Reads an Envy configuration from the env variables and launches the given action if successful.
--   (Does environment variable reading ever fail in practice? Probably not.)
withEnvConfig ∷ FromEnv α ⇒ (α → IO ()) → IO ()
withEnvConfig a = decodeEnv >>= \case
                                    Left e → hPutStrLn stderr ("error reading env: " ++ e)
                                    Right c → a c

hPutStrLn h s = liftIO $ System.IO.hPutStrLn h s

type BasicContext = (ModHttpClient, ModLogger)
type BasicApp α = MagicbaneApp BasicContext α

newBasicContext ∷ IO BasicContext
newBasicContext = do
  http ← newHttpClient
  (_, logg) ← newLogger $ LogStdout defaultBufSize
  return (http, logg)
