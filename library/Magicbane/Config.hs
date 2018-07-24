{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Trustworthy, NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, LambdaCase #-}

-- | Utility functions and reexports for System.Envy, an environment variable config reader.
module Magicbane.Config (
  module X
, module Magicbane.Config
) where

import qualified System.Envy
import           System.Envy as X hiding ((.=), (.!=), decode)
import           System.IO (stderr)
import           Magicbane.Util (hPutStrLn)

decodeEnvy = System.Envy.decode

-- | Reads an Envy configuration from the env variables and launches the given action if successful.
--   (Does environment variable reading ever fail in practice? Probably not.)
withEnvConfig ∷ FromEnv α ⇒ (α → IO ()) → IO ()
withEnvConfig a = decodeEnv >>= \case
                                    Left e → hPutStrLn stderr ("error reading env: " ++ e)
                                    Right c → a c
