module Dates ( Year
             , Month
             , Day
             , dateTimeZone
             , getCurrentDate
             , getMonthFromDay
             , monthDay
             , setToCurrentYear
             , toDate
             ) where

import Data.Text (Text, unpack)
import Data.Time ( Day
                 , defaultTimeLocale
                 , fromGregorian
                 , getCurrentTime
                 , parseTimeM
                 , toGregorian
                 , utctDay
                 )

type Year  = Integer
type Month = Int
type DateFormat = String

monthDay :: DateFormat
monthDay = "%b %d"

dateTimeZone :: DateFormat
dateTimeZone = "%a, %d %b %Y %X %z"

getCurrentDate :: IO (Year, Month, Int)
getCurrentDate = toGregorian . utctDay <$> getCurrentTime

getMonthFromDate :: (a, Month, b) -> Int
getMonthFromDate (_, month, _) = month

getMonthFromDay :: Day -> Month
getMonthFromDay = getMonthFromDate . toGregorian

setToCurrentYear :: Year -> Day -> Day
setToCurrentYear currentYear date = fromGregorian currentYear month day
  where (_, month, day) = toGregorian date

toDate :: DateFormat -> Text -> IO Day
toDate dateFormat d = case toDay dateFormat (unpack d) of
  Just d' -> return d'
  Nothing -> utctDay <$> getCurrentTime

toDay :: DateFormat -> String -> Maybe Day
toDay = parseTimeM True defaultTimeLocale