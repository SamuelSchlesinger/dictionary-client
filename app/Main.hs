{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (statusCode)
import Control.Monad (forM_)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client.TLS (tlsManagerSettings, mkManagerSettings, newTlsManagerWith)
import GHC.Generics (Generic, Rep)
import Servant.Client (client, ClientM, runClientM, mkClientEnv, BaseUrl(BaseUrl), Scheme(Https), ClientError(FailureResponse), responseStatusCode)
import System.Exit (exitFailure)
import Servant.API
import Data.Aeson (ToJSON(..), FromJSON(..), Value, Options(..), defaultOptions, GToJSON', Encoding, Zero, genericToEncoding, genericToJSON)
import Data.Text (Text)
import qualified Data.Text as Text

newtype Json a = Json { unJson :: a }

jsonOptions :: Options
jsonOptions = defaultOptions
  { omitNothingFields = True
  }

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Json a) where
  toJSON = genericToJSON jsonOptions . unJson
  toEncoding = genericToEncoding jsonOptions . unJson 

data Pronunciation = Pronunciation
  { text :: Maybe Text
  , audio :: Maybe Text
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data Meaning = Meaning
  { partOfSpeech :: Text
  , definitions :: [Definition]
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data Definition = Definition
  { definition :: Maybe Text
  , example :: Maybe Text
  , synonyms :: [Text]
  , antonyms :: [Text]
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data WordDescription = WordDescription
  { word :: Text
  , phonetics :: [Pronunciation]
  , meanings :: [Meaning]
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

type Api =
  "api" :> "v2" :> "entries" :> "en"
    :> Capture "word" Text
    :> Get '[JSON] [WordDescription]

getDefinitions :: Text -> ClientM [WordDescription]
getDefinitions = client (Proxy @Api)

main :: IO ()
main = do
  manager <- newTlsManagerWith tlsManagerSettings
  let
    clientEnv = mkClientEnv manager baseUrl
    baseUrl = BaseUrl Https "api.dictionaryapi.dev" 443 ""
  words <- (fmap Text.pack . lines) <$> readFile "/usr/share/dict/words"
  forM_ words \word ->
    runClientM (getDefinitions word) clientEnv >>= \case
      Left (FailureResponse _ (statusCode . responseStatusCode -> 404)) -> pure ()
      Left f -> print (word, f) >> liftIO exitFailure
      Right ds -> print ds
