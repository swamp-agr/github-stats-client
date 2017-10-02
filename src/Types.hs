{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Control.Exception
import Data.List (intercalate)
import Data.Monoid
import Data.Text hiding (intercalate)
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
    } deriving (Show, Generic)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    location <- o .: "location"
    token <- o .: "token"
    ua <- o .: "ua"
    constHeader <- o .: "constHeader"
    return Settings {..}

type Command = String

data Validation = VALID | INVALID deriving Show
data User =
  User
    { login :: Text
    , uid :: Integer
    , avatarUrl :: Text
    , gravatarId :: Text
    , url' :: Text
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
    }

instance Show User where
  show (User l uid a gra url h fer fig gist star subs o r e re t adm score) =
    intercalate ";" $ fmap unpack [l, pack $ show uid, a, gra, url, h, fer, fig, gist, star, subs, o, r, e, re, t, pack $ show adm, pack $ show score]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    login             <- o .: "login"
    uid               <- o .: "id"
    avatarUrl         <- o .: "avatar_url"
    gravatarId        <- o .: "gravatar_id"
    url'               <- o .: "url"
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
    } deriving Show

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

type Days = Integer

type URL = String

githubUrl :: URL
githubUrl = "https://api.github.com/search/users"

data Interaction =
  Interaction
    { limit :: Int
    , remaining :: Int
    , epoch :: Integer
    }

di = Interaction 30 0 (toInteger 0)

data CurlOptions = 
  CurlOptions
    { cHeader :: [(CurlHeader,CurlHeader)]
    , cParam :: [(CurlParam, CurlParam)]
    , cUrl :: URL
    } deriving Show

type CurlHeader = String
type CurlParam = String