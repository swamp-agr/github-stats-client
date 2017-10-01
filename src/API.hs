module API where

import Control.Exception
import Data.ByteString (ByteString(..))
import Data.Monoid
import Data.Time.Clock
import Network.Wreq

import Types

-- | First goal. Calculate all possible ranges based on total user amounts. 
getAllRanges :: Settings -> IO [Range]
getAllRanges settings = do
  let opts = setOpts settings
  -- get all users amount
  today <- getCurrentTime
  let wholeRange  = getWholeRange today
      allYearRanges  = splitRangeBy 365 wholeRange

  countByYear <- callRepeatedly (getUsersCountByRange opts) allYearRanges
  let (yearRanges, tbdYearRanges) = spanRanges countByYear
      allMonthRanges = concat $ fmap (splitRangeBy 30) tbdYearRanges

  countByMonth <- callRepeatedly (getUsersCountByRange opts) allMonthRanges
  let (monthRanges, tbdMRanges) = spanRanges countByMonth
      allWeekRanges = concat $ fmap (splitRangeBy 7) tbdMRanges

  countByWeek <- callRepeatedly (getUsersCountByRange opts) allWeekRanges
  let (weekRanges, tbdWRanges) = spanRanges countByWeek
      allDayRanges = concat $ fmap (splitRangeBy 1) tbdWRanges

  countByDay <- callRepeatedly (getUsersCountByRange opts) allDayRanges
  let (dayRanges, tbdDRanges) = spanRanges countByDay

  putStrLn $ showWarning tbdDRanges

  return $ yearRanges ++ monthRanges ++ weekRanges ++ dayRanges

getWholeRange :: UTCTime -> Range
getWholeRange t = Range defDay t

splitRangeBy :: Integer -> Range -> [Range]
splitRangeBy t rng = undefined

callRepeatedly :: (a -> IO b) -> [a] -> IO [b]
callRepeatedly f x = undefined

spanRanges :: [(Range, Int)] -> ([Range], [Range])
spanRanges x = (fmap fst $ filter ((<= 1000) . snd) x, fmap fst $ filter (not . (<= 1000) . snd) x)

showWarning :: [Range] -> String
showWarning x = unlines . fmap ((<> " period had a massive users' load!") . show) $ x

getAllUsersCount :: Options -> IO Int  
getAllUsersCount opts = do
  (rs,_) <- getCountRequest opts defRange
  return $ totalCount rs

getUsersCountByRange :: Options -> Range -> IO (Range, Int)
getUsersCountByRange opts rng = do
  (rs,_) <- getCountRequest opts rng
  let tc = totalCount rs
  putStrLn $ "For Range " <> (show rng) <> " created: " <> (show tc) <> " users!"
  return (rng, tc)

getCountRequest :: Options -> Range -> IO (GithubResponse,Options)
getCountRequest = undefined


getGithubResponseFromRange :: Settings -> Range -> IO GithubResponse
getGithubResponseFromRange settings range = do
  let opts = setOpts settings
  resp <- try $ call opts range
  case resp of
    Right result -> return $ decodeGithubResponse result
    Left err@(SomeException _)     -> return defaultResponse

getAllUsers :: Settings -> [Range] -> IO [User]
getAllUsers settings ranges = do
  let opts = setOpts settings
  rs <- callMany opts ranges
  return $ concat $ fmap fromEither rs

call :: Options -> Range -> IO ByteString
call opts range = undefined

callMany :: Options -> [Range] -> IO [APIResponse]
callMany opts ranges = undefined

decodeGithubResponse :: ByteString -> GithubResponse
decodeGithubResponse resp = undefined

setOpts :: Settings -> Options
setOpts settings = undefined

fromEither :: APIResponse -> [User]
fromEither (Right x) = items x
fromEither (Left _) = []