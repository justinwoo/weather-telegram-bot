module Main where

import Prelude
import Control.Monad.Eff.Exception as EffE
import Control.Monad.Aff (attempt, Canceler, Aff, launchAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, EXCEPTION, message)
import Control.Monad.Eff.Ref (REF)
import Control.XStream (fromAff, bindEff, Listener, addListener, create, Stream, STREAM)
import Data.Either (fromRight, Either(Left, Right))
import Data.Foreign (ForeignError, parseJSON)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (index, prop)
import Data.String.Regex (Regex, noFlags, regex)
import Global (encodeURI)
import Network.HTTP.Affjax (AJAX, get)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import TelegramBot (sendMessage, TELEGRAM, Bot, addMessagesListener, connect, Token)

type Config =
  { token :: Token
  , location :: String
  }

parseConfig :: String -> Either ForeignError Config
parseConfig string = do
  json <- parseJSON string
  token <- readProp "token" json
  location <- readProp "location" json
  pure
    { token
    , location
    }

connect' :: forall e.
  String ->
  Aff (telegram :: TELEGRAM | e) Bot
connect' = liftEff <<< connect

bindEff' :: forall e a b.
  Stream a ->
  (a -> Eff (stream :: STREAM | e) (Stream b)) ->
  Aff (stream :: STREAM | e) (Stream b)
bindEff' s p = liftEff $ s `bindEff` p

getRegex :: Regex
getRegex = unsafePartial $ fromRight $ regex "^get$" (noFlags {ignoreCase = true})

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

addListener' :: forall e a.
  Listener e a ->
  Stream a ->
  Aff (stream :: STREAM | e) Unit
addListener' listener stream = liftEff do
  addListener listener stream

type Weather =
  { city :: String
  , temp :: String
  , description :: String
  , high :: String
  , low :: String
  }

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

main :: forall e.
  Eff
    ( err :: EXCEPTION
    , fs :: FS
    , telegram :: TELEGRAM
    , ref :: REF
    , ajax :: AJAX
    , stream :: STREAM
    , console :: CONSOLE
    | e
    )
    (Canceler
       ( fs :: FS
       , telegram :: TELEGRAM
       , ref :: REF
       , ajax :: AJAX
       , stream :: STREAM
       , console :: CONSOLE
       | e
       )
    )
main = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "config.json"
  case config of
    Right {token, location} -> do
      bot <- connect' token
      requests <- addMessagesListener' bot
      results <- requests `bindEff'` (getWeather location)
      addListener'
        { next: sendMessage' bot
        , error: message >>> log
        , complete: const $ pure unit
        }
        results
    Left err -> do
      error $ "config.json is malformed: " <> (show err)
