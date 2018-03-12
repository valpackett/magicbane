{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-}

-- | Extends Servant with context.
--   Basically wrapping Servant in a ReaderT of your type.
--   Which should be a tuple of all your moudles and configs and stuff, so that the Data.Has module would let you access these items by type.
module Magicbane.App (
  module X
, module Magicbane.App
) where

import           Control.Monad.Reader
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift
import           UnliftIO.Exception as X hiding (Handler)
import           Data.Proxy as X
import           Data.Has as X
import           Servant as X hiding (And)
import qualified Servant

newtype MagicbaneApp β α = MagicbaneApp {
  unMagicbaneApp ∷ ReaderT β IO α
} deriving (Functor, Applicative, Monad,
            MonadIO, MonadBase IO, MonadBaseControl IO, MonadUnliftIO,
            MonadThrow, MonadCatch, MonadMask,
            MonadReader β)

runMagicbaneHandler ∷ β → MagicbaneApp β α → Servant.Handler α
runMagicbaneHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unMagicbaneApp a) ctx

#if MIN_VERSION_servant_server(0,12,0)
#else
magicbaneToHandler ∷ β → MagicbaneApp β :~> Servant.Handler
#if MIN_VERSION_servant_server(0,10,0)
magicbaneToHandler ctx = NT $ runMagicbaneHandler ctx
#else
magicbaneToHandler ctx = Nat $ runMagicbaneHandler ctx
#endif
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
