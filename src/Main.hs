{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), encodeFile, (.=), object)
import           Data.List (nub, sort)
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

-- add score to type as Maybe Double
data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Day
  } deriving (Generic, Eq, Show)

-- instance will hide score
instance ToJSON Album where
  toJSON Album{..} = object
    [ "artist" .= artist
    , "title"  .= title
    , "date"   .= date
    ]

instance Ord Album where
  compare (Album a1 _ d1) (Album a2 _ d2) = compare d2 d1 <> compare a1 a2

getStereogumAlbums :: IO [Album]
getStereogumAlbums = do
  cursor <- getXmlCursor "https://www.stereogum.com/heavy-rotation/feed/"
  let albumsAwaitingDate = toAlbumsAwaitingDate "–" $ getArtistsAndTitles cursor
  let dates = getReleaseDates cursor
  return $ zipWith (\album date -> album date) albumsAwaitingDate dates

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let albumsAwaitingDate = toAlbumsAwaitingDate ":" $ getArtistsAndTitles cursor
  let dates  = getReleaseDates cursor
  let albums = zipWith (\album date -> album date) albumsAwaitingDate dates
  scores <- getReviewScores cursor
  return $ filterAlbumsByScore 7.8 scores albums

getMetacriticAlbums :: IO [Album]
getMetacriticAlbums = do
  cursor <- getXmlCursor "https://www.metacritic.com/browse/albums/release-date/new-releases/date"
  let scores = getScores cursor
  let albums = getAlbums cursor
  return $ filterAlbumsByScore 80 scores albums
  where
    getAlbums cursor = zipWith3 Album (getArtists cursor) (map T.strip $ getTitles cursor) (getDates cursor)
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
    getScores cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat product_score brief_metascore"
        &/ element "div"
        &// content

filterAlbumsByScore :: Double -> [Text] -> [Album] -> [Album]
filterAlbumsByScore lowestScore scores albums = filtered
  where
    pairs :: [(Text, Album)]
    pairs = zip scores albums
    filtered :: [Album]
    filtered = map snd $ filter scoreIsHighEnough pairs
    scoreIsHighEnough :: (Text, Album) -> Bool
    scoreIsHighEnough (score, _) = readScore score >= lowestScore
    readScore :: Text -> Double
    readScore = read . T.unpack

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

getReviewScores :: Cursor -> IO [Text]
getReviewScores = mapConcurrently score . getReviewLinks
  where
    score :: Text -> IO Text
    score link = getScore <$> (parseRequest (T.unpack link) >>= getXmlCursor)

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

toAlbumsAwaitingDate :: Text -> [Text] -> [Day -> Album]
toAlbumsAwaitingDate splitter =
  map (toAlbumAwaitingDate splitter . map T.strip . T.splitOn splitter)

toAlbumAwaitingDate :: Text -> [Text] -> (Day -> Album)
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