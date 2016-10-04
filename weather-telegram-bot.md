In this post, we'll write a Telegram bot to report the weather and go over a wide breadth of features that should get you acquainted with Purescript and some of its libraries. Hopefully this will whet your appetite for writing some Purescript and become more familiar with how pure functional programming looks and works.

## Writing the Code

Our bot will need to do five things:

* Read a config file
* Create a bot instance
* Be able to receive messages and use regex to match them
* Fetch weather data
* Send the weather data as a response

We'll go over these one by one and put our whole program together.

### Reading config

While this doesn't sound glorious, this in itself introduces multiple things we'll need to get into.

#### Async

Reading a file in a Node.js environment is asynchronous, meaning that we need some way to pass in a function to continue running our program. While we could do this with callbacks, let's use a cleaner way to do this so that we can use more powerful, general techniques. The library normally used for such single-value async in Purescript is `purescript-aff`, which provides some utilities for running our program as an asynchronous one, and allows us to use async using `Aff`s freely within.

```haskell
main = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "config.json"
```

So here, `launchAff` lets us, well, launch some `Aff`s, and this first line runs `readTextFile` from `purescript-node-fs-aff`, which is an `Aff`, in order to get our config out. But we've probably already seen some things that are unfamiliar, so let's go over them.

The `<$>` operator is an alias for `map`, which you may know from `Array#map` in Javascript and such. In Purescript, there is a general interface for types that can be mapped to, which is generally defined as a function that takes a function for transforming what is inside and retaining the container, such that for an Array, it maps the function into the elements of the array, and for `Aff`, it maps the function into the resolved value of the `Aff`.

So after we read the file, the result inside of our `Aff` is mapped to a function that will parse the config file contents.

#### Parsing our config

Our `config.json` file will look something like this:

```js
{
  "token": "123456:ABCDEFGHI", // the token you get from the Telegram bot API
  "location": "Helsinki, FI" // the location we'll be getting weather for
}
```

And so we can represent it with a Record type (like Objects in Javascript) like so:

```haskell
type Token = String -- just a more descriptive type alias

type Config =
  { token :: Token
  , location :: String
  }
```

And so with this, we can then parse our JSON and start reading properties from it using `purescript-foreign`.

```haskell
parseConfig :: String -> Either ForeignError Config
parseConfig string = do
  json <- parseJSON string
  token <- readProp "token" json
  location <- readProp "location" json
  pure
    { token
    , location
    }
```

The type signature for this function is `Either ForeignError Config`, because while parsing our foreign JSON object, we can end up with all kinds of errors. If the string we pass in is an invalid JSON blob, then this will error out, and the same will happen if one of the properties we try to read either don't match the type we provide or don't exist. Likewise, `parseJSON` and `readProp` have types of `Either ForeignError _` because each of those operations can fail and immediately return the appropriate `ForeignError`.

The `pure` at the end wraps our `Config` value into the `Either`, which in this case makes it `Right Config` to represent a successful parsing operation. `pure` is like `Array.of` or `Promise.resolve` where it wraps the value into a container.

And so with this, we have our `Aff (Either ForeignError Config)` value which is then resolved using our do-block binding in our `Aff` "context".

```haskell
main = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "config.json"
  case config of
    Right {token, location} -> do
      _
    Left err -> do
      error $ "config.json is malformed: " <> (show err)
```

### Creating a bot instance

Using `purescript-node-telegram-bot-api`, we get the function `connect` with type `Token -> Eff (TelegramEffects e) Bot`, but that's not exactly the type we need, as we need to run this function in the `Aff` context. Luckily for us, there's a function for this kind of need: `liftEff`. This allows us to "lift" our function of `Eff` into an `Aff`:

```haskell
connect' :: forall e.
  String ->
  Aff (telegram :: TELEGRAM | e) Bot
connect' = liftEff <<< connect
```

And then in that block above under the `Right Config` case, we can get our bot:

```haskell
case config of
  Right {token, location} -> do
    bot <- connect' token
```

### Receiving messages and matching with regex

In order to handle multiple messages over time, we're going to have to use some kind of multi-value async type. For this, we can use `purescript-xstream`, which is a layer over the `xstream` library that Andre wrote.

Like before, this library provides a `create` function for creating a `Stream` using an `Eff`, so we can `liftEff` to make this an `Aff`.

```haskell
addMessagesListener' :: forall e.
  Bot ->
  Aff
    ( stream :: STREAM
    , telegram :: TELEGRAM
    , console :: CONSOLE
    | e
    )
    (Stream Int)
addMessagesListener' bot = liftEff do
  create
    { start: \listener -> do
        addMessagesListener bot getRegex \message string -> do
          let sender = message.from.id
          log $ "Request from " <> (show sender)
          listener.next message.from.id
    , stop: const $ pure unit
    }
```

This passes a `Listener`, which is a Record of `start` and `stop` functions, to the `create` function. This `start` function will be called when a listener is added to the stream, when the `addMessagesListener` will be called on the bot, a regex pattern, and a callback that will send the sender's id number to the listener. When the listener is removed, the `stop` will be called, which in this will do nothing by always returning `pure unit`.

The regex pattern used here is prepared in such a fashion:

```haskell
getRegex :: Regex
getRegex = unsafePartial $ fromRight $ regex "^get$" (noFlags {ignoreCase = true})
```

Constructing regex is an operation which can fail when an invalid pattern is passed in. This code only extracts the successful value. Because the pattern used is only valid for a subset of all possible string values, the value returned is given a `Partial` constraint. This constraint is unsafely removed, which means that our program will simply crash when an invalid pattern is used (normally resulting in the `Left`). But if this can crash our program, of course we need to do some work to make sure it doesn't crash.

Using `purescript-test-unit`, we can test our regex pattern such that it doesn't crash our program and that it does match the values we want to match. Thankfully, this is fairly straightforward:

```haskell
testPattern pattern string =
  equal true $ Regex.test pattern string

  main = runTest do
    suite "getRegex" do
      test "does not crash the program" do
        let testPattern' = testPattern getRegex
        testPattern' "get"
      test "tests get 'get' but with case insensitivity" do
        let testPattern' = testPattern getRegex
        testPattern' "get"
        testPattern' "Get"
        testPattern' "GET"
        expectFailure "aget shouldn't work" $ testPattern' "aget"
        expectFailure "geta shouldn't work" $ testPattern' "geta"
```

### Fetch weather data

To fetch our weather data, we'll use the Yahoo Weather API as it's quite easy to use and doesn't require any registration. The gist of this comes down to the type signatures:

```haskell
type Result =
  { id :: Int
  , weather :: Either Error Weather
  }

getWeather :: forall e.
  String ->
  Int ->
  Eff
    ( stream :: STREAM
    , ref :: REF
    , ajax :: AJAX
    | e
    )
    (Stream Result)
```

It's an `Eff` function that will return a `Stream` of `Result`, where the weather property data we fetch may end up in an error (if we can't make a successful request or parse the response). Then the rest follows:

```haskell
getWeather location id = fromAff do
  attempt' $ do
    let url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22"
              <> encodeURI location
              <> "%22)%20and%20u=%27c%27&format=json"
    response <- get url
    pure $ extractForecast response
  where
    result weather =
      pure
        { id
        , weather
        }
    attempt' aff = do
      outcome <- attempt aff
      case outcome of
        Left e ->
          result $ Left e
        Right (Left e) ->
          result (Left $ EffE.error (show e))
        Right (Right weather) ->
          result (Right weather)
```

We build a url and make a GET request, for which we extract the forecast data and wrap it in a `Stream`. The main body operates on just the `Right Weather` case, and errors will be spit out into `Left _` if there is a problem.

`purescript-aff` provides us an `attempt` function which lets us attempt running `Aff`s. We use this to run our `Aff` and handle the result as needed to make it match our `Result` type.

Our JSON parsing code is similar to the code above for how we parsed our config:

```haskell
extractForecast :: forall t.
  { response :: String | t }
  -> Either ForeignError Weather
extractForecast response = do
  json <- parseJSON response.response
  channel <-
    prop "query" >=>
    prop "results" >=>
    prop "channel" $
    json
  city <- prop "location" >=> readProp "city" $ channel
  item <- readProp "item" channel
  condition <- readProp "condition" item
  temp <- readProp "temp" condition
  description <- readProp "text" condition
  forecast <- readProp "forecast" item
  forecastToday <- index 0 forecast
  high <- readProp "high" forecastToday
  low <- readProp "low" forecastToday
  pure
    { city
    , temp
    , description
    , high
    , low
    }
```

The `>=>` operator is just an alias for "composeKleisli", which just means that we will apply this operation in succession on the argument supplied, which lets us chain operations that take an argument. It looks scary, but is really just a shorter way to write bindings in succession without having to have explicit bindings.

To use our getWeather function, we need to use a bind/flatMap operation to do so. xstream provides a `bindEff` function, which we can fit to meet our uses like so:

```haskell
bindEff' :: forall e a b.
  Stream a ->
  (a -> Eff (stream :: STREAM | e) (Stream b)) ->
  Aff (stream :: STREAM | e) (Stream b)
bindEff' s p = liftEff $ s `bindEff` p
```

So just like the other cases where we lifted `Eff` into `Aff`. As the signature says, the projecting function passed in will produce a `Stream b`, and the output will be `Stream b`. This is because bind/flatMap operations will "concatenate" values emitted by the "inner" stream into our stream. And so we can get our results stream:

```haskell
results <- requests `bindEff'` (getWeather location)
```

### Send the weather data as a response

To then add a listener to our stream and make the stream run to actually add a message listener and such, we use the `addListener` function from xstream the same way as before:

```haskell
addListener' :: forall e a.
  Listener e a ->
  Stream a ->
  Aff (stream :: STREAM | e) Unit
addListener' listener stream = liftEff do
  addListener listener stream
```

Which is then used like so:

```haskell
addListener'
  { next: sendMessage' bot
  , error: message >>> log
  , complete: const $ pure unit
  }
  results
```

Like before, `error` composes a function to extract messages from errors with a logging function, and we do nothing on `complete`. Our `next` handler has the interesting bit in which we actually prepare messages and send them. We partially apply our bot instance here so that the result can be sent to it correctly. Then our `sendMessage'` definition sends errors as-is and formats the weather to send to the user:

```haskell
sendMessage' :: forall e.
  Bot ->
  Result ->
  Eff
    ( telegram :: TELEGRAM
    , console :: CONSOLE
    | e
    )
    Unit
sendMessage' bot {id, weather} =
  case weather of
    Left e -> do
      let message = show e
      log message
      send message
    Right weather' -> do
      let message = "It's now " <> weather'.temp <> " degrees and " <>
                    weather'.description <> " in " <> weather'.city <> ".\n" <>
                    "Today's high/low is " <> weather'.high <> "/" <> weather'.low <> "."
      log message
      send message
  where
    send = sendMessage bot id
```

## Conclusion

Now we have a Telegram bot that will respond to any variation of "get" with Weather information for Helsinki! We've covered a wide range of features and tools that Purescript and its ecosystem come with, and I hope this has piqued your interest in writing some Purescript for *any* kind of application you want to make, whether it's also a Telegram bot, a general Node.js application, a front-end web application, or something on any environment that runs Javascript.

You can find the code for this post here: [github.com/justinwoo/weather-telegram-bot](https://github.com/justinwoo/weather-telegram-bot)
