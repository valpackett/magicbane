#!/usr/bin/env stack
{- stack runghc --package magicbane -- +RTS -T -RTS -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DeriveGeneric, DataKinds, TypeOperators, TemplateHaskell #-}
import Magicbane
import ClassyPrelude hiding (async)

data LargeAppConf = LargeAppConf
  {              metricsPort ∷ Int
  ,              metricsBind ∷ ByteString
  } deriving (Generic, Show)

instance Default LargeAppConf where
  def = LargeAppConf {
        metricsPort = 8090
      , metricsBind = "0.0.0.0" }

instance FromEnv LargeAppConf where
  fromEnv = gFromEnvCustom Option { dropPrefixCount = 0, customPrefix = "LARGEAPP" }
  -- so config environment variables are like LARGEAPP_METRICS_PORT

type HelloRoute = "hello" :> QueryParam "to" Text :> Get '[PlainText] Text
type SplinesRoute = "splines" :> QueryParam "count" Int :> Get '[PlainText] Text
type ExampleAPI = HelloRoute :<|> SplinesRoute
exampleAPI = Proxy ∷ Proxy ExampleAPI

type LargeAppCtx = (ModLogger, ModMetrics, LargeAppConf)
type LargeApp = MagicbaneApp LargeAppCtx

hello ∷ Maybe Text → LargeApp Text
hello x = return $ "Hello " ++ (fromMaybe "anonymous" x) ++ "!"

splines ∷ Maybe Int → LargeApp Text
splines t = timed "app.important_work" $ do -- this easy to time an action
  h ← askOpt metricsBind -- notice how the LargeAppConf type isn't mentioned anywhere except, well, the type of metricsBind? :)
  p ← askOpt metricsPort -- yes, you can read properties (== call functions on) different parts of the context!
  $logWarn $ "Reticulating splines... Check metrics at " ++ cs h ++ ":" ++ tshow p
  async $ do -- this easy to fork off a background job
    let bgTime = (fromMaybe 1 t) * 2
    threadDelay $ bgTime * 1000000
    $logDebug $ "Also done some background work in " ++ tshow bgTime ++ " second(s)"
  let fgTime = (fromMaybe 1 t)
  threadDelay $ fgTime * 1000000
  gauge "app.important_work_last" fgTime
  $logDebug $ "Done in " ++ tshow fgTime ++ " second(s)"
  return "done"

-- This isn't a Java framework, so there's no inversion of control with magical main :)
main = withEnvConfig $ \conf → do
  (_, modLogg) ← newLogger $ LogStderr defaultBufSize

  -- In a serious app, you'd probably want ekg-influxdb/ekg-carbon/ekg-statsd/ekg-prometheus-adapter/…
  -- But this web interface is pretty good in development
  metrStore ← serverMetricStore <$> forkMetricsServer (metricsBind conf) (metricsPort conf)
  metrWai ← registerWaiMetrics metrStore -- This one for the middleware
  modMetr ← newMetricsWith metrStore -- And this one for the Magicbane app (for timed/gauge/etc. calls in your actions)

  let ctx = (modLogg, modMetr, conf)

  let waiMiddleware = metrics metrWai -- you can compose it with other middleware here
  defWaiMain $ waiMiddleware $ magicbaneApp exampleAPI EmptyContext ctx $ hello :<|> splines