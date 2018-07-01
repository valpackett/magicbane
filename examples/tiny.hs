#!/usr/bin/env stack
{- stack runghc --package magicbane -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators #-}
import RIO
import Magicbane

type HelloRoute = "hello" :> QueryParam "to" Text :> Get '[PlainText] Text
type ExampleAPI = HelloRoute
exampleAPI = Proxy ∷ Proxy ExampleAPI

hello ∷ Maybe Text → BasicApp Text
hello x = do
  let x' = fromMaybe "anonymous" x
  logInfo $ "Saying hello to " <> display x'
  return $ "Hello " <> x' <> "!"

main = do
  ctx ← newBasicContext
  defWaiMain $ magicbaneApp exampleAPI EmptyContext ctx hello
