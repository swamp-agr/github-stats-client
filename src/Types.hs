{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Control.Exception
import Data.Text
import Data.Yaml
import Data.ByteString (ByteString(..), readFile)
import GHC.Generics

data Settings = 
  Settings 
    { location :: Text
    , token :: Text
    , ua :: Text
    , constHeader :: Text
    } deriving Generic
instance FromJSON Settings

type Command = String

data Validation = VALID | INVALID
data User =
  User
    { login :: Text
    , uid :: Integer
    , avatarUrl :: Text
    , gravatarId :: Text
    , url :: Text
    , htmlUrl :: Text
    , followersUrl :: Text
    , followingUrl :: Text
    , gistsUrl :: Text
    , starredUrl :: Text
    , subscriptionsUrl :: Text
    , organizationsUrl :: Text
    , reposUrl :: Text
    , eventsUrl :: Text
    , receivedEventsUrl :: Text
    , type_ :: Text
    , siteAdmin :: Bool
    , score :: Double
    } deriving Show

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    login             <- o .: "login"
    uid               <- o .: "id"
    avatarUrl         <- o .: "avatar_url"
    gravatarId        <- o .: "gravatar_id"
    url               <- o .: "url"
    htmlUrl           <- o .: "html_url"
    followersUrl      <- o .: "followers_url"
    followingUrl      <- o .: "following_url"
    gistsUrl          <- o .: "gists_url"
    starredUrl        <- o .: "starred_url"
    subscriptionsUrl  <- o .: "subscriptions_url"
    organizationsUrl  <- o .: "organizations_url"
    reposUrl          <- o .: "repos_url"
    eventsUrl         <- o .: "events_url"
    receivedEventsUrl <- o .: "received_events_url"
    type_             <- o .: "type"
    siteAdmin         <- o .: "site_admin"
    score             <- o .: "score"
    return User {..}

data Range
data APICall

type APIResponse = Either ErrorMessage GithubResponse

data GithubResponse =
  GithubResponse
    { totalCount :: Int
    , incompleteResults :: Bool
    , items :: [User]
    }

type ErrorMessage = ByteString

defRange :: Range
defRange = undefined

firstRange :: Range
firstRange = undefined

defaultResponse :: GithubResponse
defaultResponse = undefined