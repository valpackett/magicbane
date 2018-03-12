#!/usr/bin/env stack
{- stack runghc --package magicbane --package classy-prelude -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, TemplateHaskell #-}
import Magicbane
import ClassyPrelude

type HelloRoute = "hello" :> QueryParam "to" Text :> Get '[PlainText] Text
type ExampleAPI = HelloRoute
exampleAPI = Proxy ∷ Proxy ExampleAPI

hello ∷ Maybe Text → BasicApp Text
hello x = do
  $logInfo$ "Saying hello to " ++ tshow x
  return $ "Hello " ++ (fromMaybe "anonymous" x) ++ "!"

main = do
  ctx ← newBasicContext
  defWaiMain $ magicbaneApp exampleAPI EmptyContext ctx hello
