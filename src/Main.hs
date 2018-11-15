{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (encodeFile)
import           Data.List (intercalate, nub, sort, zipWith4)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Simple (Request, httpSink)
import           Text.HTML.DOM
import           Text.Read (readMaybe)
import           Text.XML.Cursor
import           System.Environment (getArgs)

import Album ( Album(..)
             , filterAlbums
             , toPartialAlbums
             )
import Dates ( Year
             , Month
             , Day
             , dateTimeZone
             , getCurrentDate
             , monthDay
             , setToCurrentYear
             , toDate
             )

main :: IO ()
main = do
  args <- getArgs
  if null args
    then error "Missing file name!"
    else writeAlbumJSON $ head args

writeAlbumJSON :: FilePath -> IO ()
writeAlbumJSON fileName = do
  (currentYear, currentMonth, _) <- getCurrentDate
  albums <- getAlbums currentMonth currentYear
  encodeFile fileName albums

getAlbums :: Month -> Year -> IO [Album]
getAlbums currentMonth currentYear = sort . nub . join <$>
  mapConcurrently id [ getPitchforkAlbums  currentMonth
                     , getStereogumAlbums  currentMonth
                     , getMetacriticAlbums currentMonth currentYear
                     ]

getStereogumAlbums :: Month -> IO [Album]
getStereogumAlbums currentMonth = do
  cursor <- getXmlCursor "https://www.stereogum.com/heavy-rotation/feed/"
  let partialAlbums = toPartialAlbums "–" $ getArtistsAndTitles cursor
  dates <- getReleaseDates cursor
  let albums = zipWith completeAlbum partialAlbums dates
  -- 0, because we don't care about score
  pure $ filterAlbums 0 currentMonth albums
  where
    completeAlbum :: (Day -> Maybe Double -> Album)
                  -> Day
                  -> Album
    completeAlbum album date = album date Nothing

getPitchforkAlbums :: Month -> IO [Album]
getPitchforkAlbums currentMonth = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let partialAlbums = toPartialAlbums ":" $ getArtistsAndTitles cursor
  dates  <- getReleaseDates cursor
  scores <- getScores cursor
  let albums = zipWith3 completeAlbum partialAlbums dates scores
  pure $ filterAlbums 7.8 currentMonth albums
  where
    completeAlbum :: (Day -> Maybe Double -> Album)
                  -> Day
                  -> Maybe Double
                  -> Album
    completeAlbum album = album

    getScores :: Cursor -> IO [Maybe Double]
    getScores = mapConcurrently linkToScore . getReviewLinks
      where
        -- for some reason, `element "link"` does not work, so we have to look
        -- for the strings that start with "http". I hate it, but I haven't
        -- found an alternative...
        getReviewLinks :: Cursor -> [Text]
        getReviewLinks cursor = filter (T.isPrefixOf "http") elems
          where elems = cursor $// element "item" &// content

        linkToScore :: Text -> IO (Maybe Double)
        linkToScore link = parseScore . getScore
          <$> (getXmlCursor =<< parseRequest (T.unpack link))

        getScore :: Cursor -> Text
        getScore cursor = T.concat score
          where score = cursor
                  $// element "span" >=> attributeIs "class" "score"
                  &// content

getMetacriticAlbums :: Month -> Year -> IO [Album]
getMetacriticAlbums currentMonth currentYear = do
  cursor <- getXmlCursor "https://www.metacritic.com/browse/albums/release-date/new-releases/date"
  let artists = getArtists cursor
  let titles  = getTitles  cursor
  let scores  = getScores  cursor
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

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = fromDocument <$> httpSink url (const sinkDoc)

getArtistsAndTitles :: Cursor -> [Text]
getArtistsAndTitles cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDates :: Cursor -> IO [Day]
getReleaseDates cursor = traverse (toDate dateTimeZone) dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

-- It may seem weird to fromMaybe only to put it back in a Maybe
-- but if we get a malformed score, we'd rather err on the side
-- of filtering it out than letting it pass
parseScore :: Text -> Maybe Double
parseScore = pure . fromMaybe 0 . readMaybe . T.unpack