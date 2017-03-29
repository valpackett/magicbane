`An object appears at your feet! The voice of Anhur rings out: "Use my gift wisely!" a - an athame named Magicbane.`

# magicbane [![Hackage](https://img.shields.io/hackage/v/magicbane.svg?style=flat)](https://hackage.haskell.org/package/magicbane) [![Build Status](https://img.shields.io/travis/myfreeweb/magicbane.svg?style=flat)](https://travis-ci.org/myfreeweb/magicbane) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

Magicbane is a Haskell framework for developing ops-friendly, high-performance, RESTful web services.

Okay, that's [Dropwizard](http://www.dropwizard.io)'s tagline. But just like Dropwizard in the Java world, Magicbane combines the best available Haskell libraries to provide a complete web development experience that reduces bikeshedding, wheel reinvention and the number of `import` lines. Hopefully :)

In particular, Magicbane combines the following libraries:

- [classy-prelude](https://www.stackage.org/package/classy-prelude) for the Prelude.
- [Warp](https://www.stackage.org/package/warp) for HTTP.
- [Servant](http://haskell-servant.readthedocs.io/en/stable/) for REST. It lets you describe web APIs with expressive type system features and implement request handlers with simple functions. Actually somewhat similar to JAX-RS/Jersey, but instead of annotations we have types, because it's Haskell instead of Java. The main feature of Magicbane is an easy way to add *stuff* (okay, let's call it "modules") on top of Servant.
- [Aeson](https://www.stackage.org/package/aeson) for JSON.
- [data-has](https://www.stackage.org/package/data-has) for extending the app context with services (modules). That thing remotely resembles dependency injection. But it's really cool!
- [envy](https://www.stackage.org/package/envy) for configuration. [Store config in environment variables](https://12factor.net/config)!
- [fast-logger](https://www.stackage.org/package/fast-logger)+[monad-logger](https://www.stackage.org/package/monad-logger) for logging. It works. It's fast. And it even lets you see what line of code produced a log message.
- [EKG](https://www.stackage.org/package/ekg)+[monad-metrics](https://www.stackage.org/package/monad-metrics) for metrics. `monad-metrics` lets you easily measure things in your application: just use `label`/`counter`/`distribution`/`gauge`/`timed` in your handlers. The EKG ecosystem has backends for [InfluxDB](https://www.stackage.org/package/ekg-influxdb), [Carbon (Graphite)](https://www.stackage.org/package/ekg-carbon), [statsd](https://www.stackage.org/package/ekg-statsd), [Prometheus](https://www.stackage.org/package/ekg-prometheus-adapter) and others… And a simple local [web server](https://www.stackage.org/package/ekg-wai) for development.
- [refined](https://nikita-volkov.github.io/refined/) for validation. Why use functions for input validation when you can use types? Magicbane integrates `refined` with Aeson, so you can write things like `count ∷ Refined Positive Int` in your data type definitions and inputs that don't satisfy the constraints will be rejected when input is processed.
- [http-client](https://www.stackage.org/package/http-client)([-tls](https://www.stackage.org/package/http-client-tls)) for, well, making HTTP requests. Most high level HTTP client libraries are built on top of that. Magicbane provides a small composable interface based on [http-conduit](https://www.stackage.org/package/http-conduit), which lets you e.g. stream the response body directly into [an HTML parser](https://www.stackage.org/package/html-conduit).
- [http-link-header](https://www.stackage.org/package/http-link-header) for the [HTTP `Link` header](https://tools.ietf.org/html/rfc5988#section-5), unsurprisingly.
- [wai-cli](https://www.stackage.org/package/wai-cli) for starting Warp. Why write the same stuff in the `main` function for every new app when you can just use this one. It supports stuff people usually forget to implement there, like UNIX domain sockets, socket activation and graceful shutdown.

Not part of Magicbane, but recommended:

- [rapid](https://www.stackage.org/package/rapid) for fast development with GHCi hot reload.
- [hasql](https://www.stackage.org/package/hasql) for talking to PostgreSQL.
- [html-conduit](https://www.stackage.org/package/html-conduit) for parsing HTML.
- [microformats2-parser](https://www.stackage.org/package/microformats2-parser) for parsing microformats2 from that HTML.
- [pcre-heavy](https://www.stackage.org/package/pcre-heavy) for regular expressions.

Magicbane was extracted from [Sweetroll](https://github.com/myfreeweb/sweetroll).

## Usage

Here's a hello world service. Just a simple file you can launch with [stack script](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)! (Don't use stack script in production though, use proper stack builds, with optimizations and the threaded runtime.)

```haskell
#!/usr/bin/env stack
{- stack runghc --package magicbane -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, TemplateHaskell #-}
import Magicbane

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
```

This defines an API that consists of just one endpoint, `/hello?to=someone`, that does exactly what it says on the tin.
Looks like a normal Servant app, but the handler is defined as a `BasicApp` action. What's that?

That's just an example context for simple apps:

```haskell
type BasicContext = (ModHttpClient, ModLogger)
type BasicApp α = MagicbaneApp BasicContext α
```

Okay, what's `MagicbaneApp`?

```haskell
newtype MagicbaneApp β α = MagicbaneApp {
  unMagicbaneApp ∷ ReaderT β (ExceptT ServantErr IO) α }
```

It's Servant, wrapped in a `ReaderT`!
Combined with [data-has](https://www.stackage.org/package/data-has), this makes it possible to have a beautifully extensible context.

Let's make our own context instead of using the basic one:

```haskell
#!/usr/bin/env stack
{- stack runghc --package magicbane -}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators, TemplateHaskell #-}
import Magicbane

type MyAppContext = (ModLogger, ModMetrics)
type MyApp = MagicbaneApp MyAppContext

type HelloRoute = "hello" :> QueryParam "to" Text :> Get '[PlainText] Text
type ExampleAPI = HelloRoute
exampleAPI = Proxy ∷ Proxy ExampleAPI

hello ∷ Maybe Text → MyApp Text
hello x = timed "hello" $ do
  $logInfo$ "Saying hello to " ++ tshow x
  return $ "Hello " ++ (fromMaybe "anonymous" x) ++ "!"

main = do
  (_, modLogg) ← newLogger $ LogStderr defaultBufSize
  metrStore ← serverMetricStore <$> forkMetricsServer "0.0.0.0" 8800
  modMetr ← newMetricsWith metrStore
  let ctx = (modLogg, modMetr)
  defWaiMain $ magicbaneApp exampleAPI EmptyContext ctx hello
```

Now we have metrics and logging instead of HTTP client and logging!
`timed` is used here to measure how long it takes to say hello.

See the `examples` directory for more examples.

## Development

Use [stack] to build.  

```bash
$ stack build
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/4/).

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).

(However, the dependencies are not all unlicense'd!)
