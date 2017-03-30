{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-}

-- | Extends Servant with context.
--   Basically wrapping Servant in a ReaderT of your type.
--   Which should be a tuple of all your moudles and configs and stuff, so that the Data.Has module would let you access these items by type.
module Magicbane.App (
  module X
, module Magicbane.App
) where

import           ClassyPrelude
import           Control.Monad.Trans.Except as X
import           Control.Monad.Except as X (MonadError, throwError)
import           Data.Proxy as X
import           Data.Has as X
import           Servant as X hiding (And)

newtype MagicbaneApp β α = MagicbaneApp {
  unMagicbaneApp ∷ ReaderT β (ExceptT ServantErr IO) α
} deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO,
            MonadThrow, MonadCatch, MonadError ServantErr, MonadReader β)

instance MonadBaseControl IO (MagicbaneApp β) where
  type StM (MagicbaneApp β) α = StM (ReaderT β (ExceptT ServantErr IO)) α
  liftBaseWith f = MagicbaneApp $ liftBaseWith $ \x → f $ x . unMagicbaneApp
  restoreM       = MagicbaneApp . restoreM

runMagicbaneExcept ∷ β → MagicbaneApp β α → ExceptT ServantErr IO α
runMagicbaneExcept ctx a = ExceptT $ liftIO $ runExceptT $ runReaderT (unMagicbaneApp a) ctx

magicbaneToExcept ∷ β → MagicbaneApp β :~> ExceptT ServantErr IO
#if MIN_VERSION_servant_server(0,10,0)
magicbaneToExcept ctx = NT $ runMagicbaneExcept ctx
#else
magicbaneToExcept ctx = Nat $ runMagicbaneExcept ctx
#endif

-- | Constructs a WAI application from an API definition, a Servant context (used for auth mainly), the app context and the actual action handlers.
magicbaneApp api sctx ctx actions = serveWithContext api sctx $ srv ctx
  where srv c = enter (magicbaneToExcept c) actions

-- | Gets a value of any type from the context.
askObj ∷ (Has β α, MonadReader α μ) ⇒ μ β
askObj = asks getter

-- | Gets a thing from a value of any type from the context. (Useful for configuration fields.)
askOpt ∷ (Has β α, MonadReader α μ) ⇒ (β → ψ) → μ ψ
askOpt f = asks $ f . getter
