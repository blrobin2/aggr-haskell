{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), encodeFile, (.=), object)
import           Data.List (nub, sort, zipWith4)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, parseTimeM)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics (Generic)
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Simple (Request
                                     , httpLBS
                                     , httpSink
                                     , getResponseBody
                                     )
import           Text.HTML.DOM
import           Text.XML.Cursor

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Day
  , score  :: Maybe Double
  } deriving (Generic, Eq, Show)

instance ToJSON Album where
  toJSON Album{..} = object
    [ "artist" .= artist
    , "title"  .= title
    , "date"   .= date
    ]

instance Ord Album where
  compare (Album a1 _ d1 _) (Album a2 _ d2 _) = compare d2 d1 <> compare a1 a2

getStereogumAlbums :: IO [Album]
getStereogumAlbums = do
  cursor <- getXmlCursor "https://www.stereogum.com/heavy-rotation/feed/"
  let albumsAwaitingDate = toAlbumsAwaitingDate "–" $ getArtistsAndTitles cursor
  let dates = getReleaseDates cursor
  return $ zipWith (\album date -> album date Nothing) albumsAwaitingDate dates

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let albumsAwaitingDate = toAlbumsAwaitingDate ":" $ getArtistsAndTitles cursor
  let dates  = getReleaseDates cursor
  scores <- getReviewScores cursor
  let albums = zipWith3 (\album date score -> album date score) albumsAwaitingDate dates scores

  return $ filterAlbumsByScore 7.8 albums

getMetacriticAlbums :: IO [Album]
getMetacriticAlbums = do
  cursor <- getXmlCursor "https://www.metacritic.com/browse/albums/release-date/new-releases/date"
  --let scores = getScores cursor
  let albums = getAlbums cursor
  return $ filterAlbumsByScore 80 albums
  where
    getAlbums cursor = zipWith4 Album (getArtists cursor) (map T.strip $ getTitles cursor) (getDates cursor) (getScores cursor)
    getArtists cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat condensed_stats"
        &/ element "ul"
        &/ element "li" >=> attributeIs "class" "stat product_artist"
        &/ element "span" >=> attributeIs "class" "data"
        &// content
    getTitles cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_title"
        &/ element "a"
        &// content
    getDates cursor = map toDate $ cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat condensed_stats"
        &/ element "ul"
        &/ element "li" >=> attributeIs "class" "stat release_date"
        &/ element "span" >=> attributeIs "class" "data"
        &// content
    toDate d = case parseTimeM True defaultTimeLocale "%b %e" (T.unpack d) of
      Right d' -> d'
    getScores :: Cursor -> [Maybe Double]
    getScores cursor = map parseScore $ cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_score brief_metascore"
        &/ element "div"
        &// content

filterAlbumsByScore :: Double -> [Album] -> [Album]
filterAlbumsByScore lowestScore albums = filtered
  where
    filtered :: [Album]
    filtered = filter scoreIsHighEnough albums
    scoreIsHighEnough :: Album -> Bool
    scoreIsHighEnough album = case score album of
      Nothing -> True
      Just s  -> s >= lowestScore

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = do
  doc <- httpSink url $ const sinkDoc
  return $ fromDocument doc

getArtistsAndTitles :: Cursor -> [Text]
getArtistsAndTitles cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDates :: Cursor -> [Day]
getReleaseDates cursor = map toDate dates
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

toAlbumsAwaitingDate :: Text -> [Text] -> [Day -> Maybe Double -> Album]
toAlbumsAwaitingDate splitter =
  map (toAlbumAwaitingDate splitter . map T.strip . T.splitOn splitter)

toAlbumAwaitingDate :: Text -> [Text] -> (Day -> Maybe Double -> Album)
toAlbumAwaitingDate _ [a, t]    = Album a t
toAlbumAwaitingDate _ [a]       = Album a ""
toAlbumAwaitingDate splitter [a,t1,t2] = Album a (t1 <> splitter <> t2)
toAlbumAwaitingDate _ _         = error "Invalid pattern!"

dateToText :: Day -> Text
dateToText = T.pack . formatTime defaultTimeLocale "%b %d"

toDate :: Text -> Day
toDate d = case toDay (T.unpack d) of
  Just d' -> d'

toDay :: String -> Maybe Day
toDay = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

main :: IO ()
main = do
  -- get only for this month and sort by date, then title
  albums <- sort . nub . join <$>
    mapConcurrently id [ getPitchforkAlbums
                       , getStereogumAlbums
                       , getMetacriticAlbums
                       ]
  encodeFile "albums.json" albums