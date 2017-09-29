{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Exception
import Data.Yaml
import Data.ByteString (ByteString(..), readFile)
import GHC.Generics

data Settings = Settings String deriving Generic
instance FromJSON Settings

type Command = String

data Validation = VALID | INVALID
data User
instance Show User

data Range
data APICall

type APIResponse = Either ErrorMessage [User]
type ErrorMessage = ByteString

defRange :: Range
defRange = undefined