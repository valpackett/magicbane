{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts, FlexibleInstances, UndecidableInstances, ConstraintKinds #-}

-- | Provides an HTTP(S) client via http-client(-tls) in a Magicbane app context.
--   Also provides a simple composable interface for making arbitrary requests, based on http-client-conduit.
--   That lets you plug stream parsers (e.g. html-conduit: 'performWithFn (.| sinkDoc)') directly into the reading of the response body.
module Magicbane.HTTPClient (
  module Magicbane.HTTPClient
, module X
) where

import           Control.Monad.Reader
import           Control.Monad.Catch as X (MonadCatch)
import           Control.Monad.Trans.Except as X (ExceptT (..), runExceptT)
import qualified Control.Monad.Trans.Except as MTE
import           Control.Monad.IO.Unlift as X (MonadUnliftIO)
import           UnliftIO.Exception (tryAny)
import           Data.Aeson (ToJSON, encode)
import           Data.Has
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, toStrict)
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.String.Conversions
import           Data.Text (Text, pack)
import           Network.URI as X
import           Network.HTTP.Types
import           Network.HTTP.Conduit as HC
import           Network.HTTP.Client.Conduit as HCC
import           Network.HTTP.Client.Internal (setUri) -- The fuck?
import           Network.HTTP.Client as X hiding (Proxy, path)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Magicbane.Util (writeForm)

newtype ModHttpClient = ModHttpClient Manager

instance (Has ModHttpClient α) ⇒ HasHttpManager α where
  getHttpManager = (\(ModHttpClient m) → m) <$> getter

newHttpClient ∷ IO ModHttpClient
newHttpClient = ModHttpClient <$> newTlsManager

type MonadHTTP ψ μ = (HasHttpManager ψ, MonadReader ψ μ, MonadUnliftIO μ)

runHTTP ∷ ExceptT ε μ α → μ (Either ε α)
runHTTP = MTE.runExceptT

-- | Creates a request from a URI.
reqU ∷ (MonadHTTP ψ μ) ⇒ URI → ExceptT Text μ Request
reqU uri = ExceptT $ return $ bimap (pack.show) id $ setUri defaultRequest uri

-- | Creates a request from a string of any type, parsing it into a URI.
reqS ∷ (MonadHTTP ψ μ, ConvertibleStrings σ String) ⇒ σ → ExceptT Text μ Request
reqS uri = ExceptT $ return $ bimap (pack.show) id $ parseUrlThrow $ cs uri

-- | Configures the request to not throw errors on error status codes.
anyStatus ∷ (MonadHTTP ψ μ) ⇒ Request → ExceptT Text μ Request
anyStatus req = return $ setRequestIgnoreStatus req

-- | Sets a x-www-form-urlencoded form as the request body (also sets the content-type).
postForm ∷ (MonadHTTP ψ μ) ⇒ [(Text, Text)] → Request → ExceptT Text μ Request
postForm form req =
  return req { method = "POST"
             , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded; charset=utf-8") ]
             , requestBody = RequestBodyBS $ writeForm form }

-- | Sets a JSON value as the request body (via ToJSON; also sets the content-type).
postJson :: (MonadHTTP ψ μ, ToJSON α) ⇒ α → Request → ExceptT Text μ Request
postJson body req =
  return req { method = "POST"
             , requestHeaders = [ (hContentType, "application/json; charset=utf-8") ]
             , requestBody = RequestBodyBS . L.toStrict . encode $ body }

-- | Performs the request, using a given function to read the body. This is what all other performWith functions are based on.
performWithFn ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ (ConduitM ι ByteString μ () → ConduitT () Void μ ρ) → Request → ExceptT Text μ (Response ρ)
performWithFn fn req = do
  res ← lift $ tryAny $ HCC.withResponse req $ \res → do
    body ← runConduit $ fn $ responseBody res
    return res { responseBody = body }
  ExceptT $ return $ bimap (pack.show) id res

-- | Performs the request, ignoring the body.
performWithVoid ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ Request → ExceptT Text μ (Response ())
performWithVoid = performWithFn (const $ return ())

-- | Performs the request, reading the body into a lazy ByteString.
performWithBytes ∷ (MonadHTTP ψ μ, MonadCatch μ) ⇒ Request → ExceptT Text μ (Response L.ByteString)
performWithBytes = performWithFn (.| C.sinkLazy)
