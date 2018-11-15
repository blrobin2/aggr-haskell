{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (encodeFile)
import           Data.List (intercalate, nub, sort, zipWith4)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Simple (Request, httpSink)
import           Text.HTML.DOM
import           Text.XML.Cursor
import           System.Environment (getArgs)

import Album (Album(..))
import Dates ( Year
             , Month
             , Day
             , dateTimeZone
             , getCurrentDate
             , getMonthFromDay
             , monthDay
             , setToCurrentYear
             , toDate
             )

getStereogumAlbums :: Month -> IO [Album]
getStereogumAlbums currentMonth = do
  cursor <- getXmlCursor "https://www.stereogum.com/heavy-rotation/feed/"
  let partialAlbums = toPartialAlbums "–" $ getArtistsAndTitles cursor
  dates <- getReleaseDates cursor
  let albums = zipWith (\album date -> album date Nothing) partialAlbums dates
  -- 0, because we don't care about score
  pure $ filterAlbums 0 currentMonth albums

getPitchforkAlbums :: Month -> IO [Album]
getPitchforkAlbums currentMonth = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let partialAlbums = toPartialAlbums ":" $ getArtistsAndTitles cursor
  dates  <- getReleaseDates cursor
  scores <- getReviewScores cursor
  let albums = zipWith3 completeAlbum partialAlbums dates scores
  pure $ filterAlbums 7.8 currentMonth albums
  where
    completeAlbum :: (Day -> Maybe Double -> Album)
                  -> Day
                  -> Maybe Double
                  -> Album
    completeAlbum album = album

getMetacriticAlbums :: Month -> Year -> IO [Album]
getMetacriticAlbums currentMonth currentYear = do
  cursor <- getXmlCursor "https://www.metacritic.com/browse/albums/release-date/new-releases/date"
  let artists = getArtists cursor
  let titles  = getTitles cursor
  let scores  = getScores cursor
  -- Because we only get the month and day, the Day defaults to 1970
  -- So we pass in the current year so we can set it for each date
  dates <- map (setToCurrentYear currentYear) <$> getDates cursor
  let albums = zipWith4 Album artists titles dates scores
  pure $ filterAlbums 80 currentMonth albums
  where
    getArtists :: Cursor -> [Text]
    getArtists cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat condensed_stats"
        &/ element "ul"
        &/ element "li" >=> attributeIs "class" "stat product_artist"
        &/ element "span" >=> attributeIs "class" "data"
        &// content

    getTitles :: Cursor -> [Text]
    getTitles cursor = map T.strip $ cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_title"
        &/ element "a"
        &// content
    getDates :: Cursor -> IO [Day]
    getDates cursor = traverse (toDate monthDay)
        $ cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat condensed_stats"
        &/ element "ul"
        &/ element "li" >=> attributeIs "class" "stat release_date"
        &/ element "span" >=> attributeIs "class" "data"
        &// content

    getScores :: Cursor -> [Maybe Double]
    getScores cursor = map parseScore $ cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_score brief_metascore"
        &/ element "div"
        &// content

filterAlbums :: Double -> Month -> [Album] -> [Album]
filterAlbums lowestScore currentMonth = filter filterer
  where
    filterer :: Album -> Bool
    filterer = (&&) <$> cameOutThisMonth <*> scoreIsHighEnough

    scoreIsHighEnough :: Album -> Bool
    scoreIsHighEnough album = case score album of
      Nothing -> True
      Just s  -> s >= lowestScore

    cameOutThisMonth :: Album -> Bool
    cameOutThisMonth album = (getMonthFromDay . date $ album) == currentMonth

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = do
  doc <- httpSink url $ const sinkDoc
  pure $ fromDocument doc

getArtistsAndTitles :: Cursor -> [Text]
getArtistsAndTitles cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDates :: Cursor -> IO [Day]
getReleaseDates cursor = traverse (toDate dateTimeZone) dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

getReviewScores :: Cursor -> IO [Maybe Double]
getReviewScores = mapConcurrently score . getReviewLinks
  where
    score :: Text -> IO (Maybe Double)
    score link = parseScore . getScore
      <$> (parseRequest (T.unpack link) >>= getXmlCursor)

-- for some reason, `element "link"` does not work, so we have to look for
-- the strings that start with "http". I hate it, but I haven't found
-- an alternative...
getReviewLinks :: Cursor -> [Text]
getReviewLinks cursor = filter (T.isPrefixOf "http") elems
  where elems = cursor $// element "item" &// content

getScore :: Cursor -> Text
getScore cursor = T.concat score
  where score = cursor
          $// element "span" >=> attributeIs "class" "score"
          &// content

parseScore :: Text -> Maybe Double
parseScore s
  -- We have multiple albums, and therefore a likely a reissue
  -- so we'll just filter it out
  | length stringScore > 3 = pure 0
  | otherwise = pure $ read stringScore
  where stringScore = T.unpack s

toPartialAlbums :: Text -> [Text] -> [Day -> Maybe Double -> Album]
toPartialAlbums splitter =
  map (toPartialAlbum splitter . map T.strip . T.splitOn splitter)

toPartialAlbum :: Text -> [Text] -> (Day -> Maybe Double -> Album)
toPartialAlbum _ [a, t] = Album a t
toPartialAlbum _ [a]    = Album a ""
toPartialAlbum splitter [a,t1,t2] = Album a (t1 <> splitter <> t2)
toPartialAlbum splitter xs = error $ errorString splitter xs
  where
    errorString :: Text -> [Text] -> String
    errorString s xs = "Invalid pattern: "
      <> intercalate (T.unpack s) (map T.unpack xs)

getAlbums :: Month -> Year -> IO [Album]
getAlbums currentMonth currentYear = sort . nub . join <$>
  mapConcurrently id [ getPitchforkAlbums  currentMonth
                     , getStereogumAlbums  currentMonth
                     , getMetacriticAlbums currentMonth currentYear
                     ]

writeAlbumJSON :: FilePath -> IO ()
writeAlbumJSON fileName = do
  (currentYear, currentMonth, _) <- getCurrentDate
  albums <- getAlbums currentMonth currentYear
  encodeFile fileName albums

main :: IO ()
main = do
  args <- getArgs
  if null args
    then error "Missing file name!"
    else writeAlbumJSON $ head args