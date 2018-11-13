{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (encodeFile)
import           Data.List (nub, sort, zipWith4)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Simple (Request, httpSink)
import           Text.HTML.DOM
import           Text.XML.Cursor

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
  let dates = getReleaseDates cursor
  let albums = zipWith (\album date -> album date Nothing) partialAlbums dates
  -- 0, because we don't care about score
  return $ filterAlbums 0 currentMonth albums

getPitchforkAlbums :: Month -> IO [Album]
getPitchforkAlbums currentMonth = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let partialAlbums = toPartialAlbums ":" $ getArtistsAndTitles cursor
  let dates = getReleaseDates cursor
  scores <- getReviewScores cursor
  let albums = zipWith3 completeAlbum partialAlbums dates scores
  return $ filterAlbums 7.8 currentMonth albums
  where
    completeAlbum :: (Day -> Maybe Double -> Album)
                  -> Day
                  -> Maybe Double
                  -> Album
    completeAlbum album = album

getMetacriticAlbums :: Month -> Year -> IO [Album]
getMetacriticAlbums currentMonth currentYear = do
  cursor <- getXmlCursor "https://www.metacritic.com/browse/albums/release-date/new-releases/date"
  let albums = getAlbums cursor
  return $ filterAlbums 80 currentMonth albums
  where
    getAlbums :: Cursor -> [Album]
    getAlbums cursor = zipWith4 Album
      (getArtists cursor)
      (map T.strip $ getTitles cursor)
      (getDates cursor currentYear)
      (getScores cursor)

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
    getTitles cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_title"
        &/ element "a"
        &// content
    -- Because we only get the month and day, the Day defaults to 1970
    -- So we pass in the current year so we can set it for each date
    getDates :: Cursor -> Year -> [Day]
    getDates cursor currentYear = map (setToCurrentYear currentYear . toDate monthDay)
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
  return $ fromDocument doc

getArtistsAndTitles :: Cursor -> [Text]
getArtistsAndTitles cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDates :: Cursor -> [Day]
getReleaseDates cursor = map (toDate dateTimeZone) dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

getReviewScores :: Cursor -> IO [Maybe Double]
getReviewScores = mapConcurrently score . getReviewLinks
  where
    score :: Text -> IO (Maybe Double)
    score link = parseScore . getScore <$> (parseRequest (T.unpack link) >>= getXmlCursor)

getReviewLinks :: Cursor -> [Text]
getReviewLinks cursor = links
  where
    -- for some reason, element "link" does not work, so we have to do this
    elems = cursor $// element "item" &// content
    links = filter (T.isPrefixOf "http") elems

getScore :: Cursor -> Text
getScore cursor = T.concat score
  where
    score =
      cursor $// element "span" >=> attributeIs "class" "score" &// content

parseScore :: Text -> Maybe Double
parseScore = Just . read . T.unpack

toPartialAlbums :: Text -> [Text] -> [Day -> Maybe Double -> Album]
toPartialAlbums splitter =
  map (toPartialAlbum splitter . map T.strip . T.splitOn splitter)

toPartialAlbum :: Text -> [Text] -> (Day -> Maybe Double -> Album)
toPartialAlbum _ [a, t]    = Album a t
toPartialAlbum _ [a]       = Album a ""
toPartialAlbum splitter [a,t1,t2] = Album a (t1 <> splitter <> t2)
toPartialAlbum _ _         = error "Invalid pattern!"

main :: IO ()
main = do
  (currentYear, currentMonth, _) <- getCurrentDate
  albums <- sort . nub . join <$>
    mapConcurrently id [ getPitchforkAlbums  currentMonth
                       , getStereogumAlbums  currentMonth
                       , getMetacriticAlbums currentMonth currentYear
                       ]
  encodeFile "albums.json" albums