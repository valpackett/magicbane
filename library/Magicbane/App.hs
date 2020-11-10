{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, TypeApplications, ScopedTypeVariables, CPP, TypeOperators, FlexibleContexts #-}

-- | Extends Servant with context, based on RIO.
--   The context should be a tuple of all your moudles and configs and stuff, so that the Data.Has module would let you access these items by type.
module Magicbane.App (
  module X
, module Magicbane.App
, RIO
) where

import           RIO
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Proxy as X
import           Servant as X hiding (And, Handler)
import qualified Servant

runMagicbaneHandler ∷ β → RIO β α → Servant.Handler α
runMagicbaneHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx

-- | Constructs a WAI application from an API definition, a Servant context (used for auth mainly), the app context and the actual action handlers.
magicbaneApp ∷ forall β χ ψ. (HasServer χ ψ, HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters) ⇒ Proxy χ → Context ψ → β → ServerT χ (RIO β) → Application
magicbaneApp api sctx ctx actions = serveWithContext api sctx $ srv ctx
  where srv c = hoistServerWithContext api (Proxy @ψ) (runMagicbaneHandler c) actions
