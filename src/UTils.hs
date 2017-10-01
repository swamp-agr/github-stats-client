{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import qualified Data.Text as T
import Data.Char
import Data.Time.Clock

import Types

toRange :: [String] -> (Validation, Range)
toRange (x:y:[]) =
  if x' < y'
  then (VALID, Range x' y')
  else ir
  where x' = parseDate defDay x
        y' = parseDate defDay y
toRange _ = ir

-- ir = invalid result
ir :: (Validation, Range)
ir = (INVALID, defRange)

isInt :: T.Text -> Bool
isInt = not . null. ignoreChars

toInt :: String -> Int
toInt a = read a :: Int

ignoreChars :: T.Text -> String
ignoreChars = filter isDigit . T.unpack

parseDefInt :: T.Text -> Int -> Int
parseDefInt x def = if isInt x then parseInt x else def

parseInt :: T.Text -> Int
parseInt = toInt . ignoreChars

parseDate def x = ymdToDate def . fmap (flip parseDefInt 0) . triple . T.pack $ x
  where triple = T.splitOn "-"

-- | we assume following assumptions on usage. Year is in range 2006..2999. Month is in 1..12. Day 1..31.
ymdToDate :: UTCTime -> [Int] -> UTCTime
ymdToDate def x =
  if length x == 3
  then if and [ x !! 0 > 2005
              , x !! 0 < 3000
              , x !! 1 > 0
              , x !! 1 < 13
              , x !! 2 > 0
              , x !! 2 < 32
              ]
       then toDate (toInteger $ x !! 0) (x !! 1) (x !! 2)
       else def
  else def