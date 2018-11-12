{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>), join)
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON
                            , defaultOptions
                            , encodeFile
                            , genericToEncoding
                            , toEncoding
                            )
import           Data.List (nub, sort)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
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

type Date = Text
-- TODO: store dates here, and then do the formatting in the the ToJSON instance
data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Date
  } deriving (Generic, Eq, Show)

instance Ord Album where
  compare (Album a1 _ d1) (Album a2 _ d2) = compare d2 d1 <> compare a1 a2

instance ToJSON Album where
  toEncoding = genericToEncoding defaultOptions

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
    getDates cursor = cursor $//
        element "li" >=> attributeIs "class" "product release_product"
        &/ element "div"
        &/ element "div" >=> attributeIs "class" "basic_stat condensed_stats"
        &/ element "ul"
        &/ element "li" >=> attributeIs "class" "stat release_date"
        &/ element "span" >=> attributeIs "class" "data"
        &// content
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

getReleaseDates :: Cursor -> [Text]
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

toAlbumsAwaitingDate :: Text -> [Text] -> [Date -> Album]
toAlbumsAwaitingDate splitter =
  map (toAlbumAwaitingDate splitter . map T.strip . T.splitOn splitter)

toAlbumAwaitingDate :: Text -> [Text] -> (Date -> Album)
toAlbumAwaitingDate _ [a, t]    = Album a t
toAlbumAwaitingDate _ [a]       = Album a ""
toAlbumAwaitingDate splitter [a,t1,t2] = Album a (t1 <> splitter <> t2)
toAlbumAwaitingDate _ _         = error "Invalid pattern!"

toDate :: Text -> Date
toDate d = case toUCTTime (T.unpack d) of
  Nothing  -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d" d'

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

main :: IO ()
main = do
  -- get only for this month and sort by date, then title
  albums <- sort . nub . join <$>
    mapConcurrently id [ getPitchforkAlbums
                       , getStereogumAlbums
                       , getMetacriticAlbums
                       ]
  encodeFile "albums.json" albums