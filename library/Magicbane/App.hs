{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-}

-- | Extends Servant with context, based on RIO.
--   The context should be a tuple of all your moudles and configs and stuff, so that the Data.Has module would let you access these items by type.
module Magicbane.App (
  module X
, module Magicbane.App
, RIO
) where

import           RIO
import           RIO.Orphans as X ()
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Proxy as X
import           Data.Has as X
import           Servant as X hiding (And, Handler)
import qualified Servant

runMagicbaneHandler ∷ β → RIO β α → Servant.Handler α
runMagicbaneHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx

#if MIN_VERSION_servant_server(0,12,0)
#else
magicbaneToHandler ∷ β → RIO β :~> Servant.Handler
magicbaneToHandler ctx = NT $ runMagicbaneHandler ctx
#endif

-- | Constructs a WAI application from an API definition, a Servant context (used for auth mainly), the app context and the actual action handlers.
magicbaneApp api sctx ctx actions = serveWithContext api sctx $ srv ctx
#if MIN_VERSION_servant_server(0,12,0)
  where srv c = hoistServer api (runMagicbaneHandler c) actions
#else
  where srv c = enter (magicbaneToHandler c) actions
#endif

-- | Gets a value of any type from the context.
askObj ∷ (Has β α, MonadReader α μ) ⇒ μ β
askObj = asks getter

-- | Gets a thing from a value of any type from the context. (Useful for configuration fields.)
askOpt ∷ (Has β α, MonadReader α μ) ⇒ (β → ψ) → μ ψ
askOpt f = asks $ f . getter
