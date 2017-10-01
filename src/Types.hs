{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Control.Exception
import Data.Monoid
import Data.Text
import Data.Time.Clock
import Data.Time.Calendar
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

data Validation = VALID | INVALID deriving Show
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

data Range = 
  Range
    { startDate :: UTCTime
    , endTIme :: UTCTime
    }

instance Show Range where
  show (Range x y) = (f x) <> ".." <> (f y)
    where f = showGregorian . utctDay

data APICall

type APIResponse = Either ErrorMessage GithubResponse

data GithubResponse =
  GithubResponse
    { totalCount :: Int
    , incompleteResults :: Bool
    , items :: [User]
    }

instance FromJSON GithubResponse where 
  parseJSON = withObject "GithubResponse" $ \o -> do
    totalCount        <- o .: "total_count"
    incompleteResults <- o .: "incomplete_results"
    items             <- o .: "items"
    return GithubResponse {..}

type ErrorMessage = ByteString

defRange :: Range
defRange = 
  Range defDay defDay

defDay :: UTCTime
defDay = toDate 2006 1 1

dayToDate :: Day -> UTCTime
dayToDate x = UTCTime x (secondsToDiffTime 0)


defaultResponse :: GithubResponse
defaultResponse = GithubResponse 0 False []

toDate y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

type Days = Int