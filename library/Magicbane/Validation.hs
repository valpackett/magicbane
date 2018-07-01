{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

-- | Integrates the refinement types from the refined library with aeson.
module Magicbane.Validation (
  module Refined
) where

import           Refined
import           Data.Aeson

instance ToJSON α ⇒ ToJSON (Refined ρ α) where
  toJSON = toJSON . unrefine

instance (FromJSON α, Predicate ρ α) ⇒ FromJSON (Refined ρ α) where
  parseJSON x = do
    res ← parseJSON x
    case refine res of
      Right v → return v
      Left e → fail $ show e
