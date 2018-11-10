{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>))
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON
                            , defaultOptions
                            , encodeFile
                            , genericToEncoding
                            , toEncoding
                            )
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

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Date
  } deriving (Generic, Eq, Show)

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
  return $ filterAlbumsByScore scores albums

filterAlbumsByScore :: [Text] -> [Album] -> [Album]
filterAlbumsByScore scores albums = filtered
  where
    pairs :: [(Text, Album)]
    pairs = zip scores albums
    filtered :: [Album]
    filtered = map snd $ filter scoreIsHighEnough pairs
    scoreIsHighEnough :: (Text, Album) -> Bool
    scoreIsHighEnough (score, _) = readScore score >= 7.8
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
toAlbumsAwaitingDate splitter = map (toAlbumAwaitingDate . map T.strip . T.splitOn splitter)

toAlbumAwaitingDate :: [Text] -> (Date -> Album)
toAlbumAwaitingDate [a, t]    = Album a t
toAlbumAwaitingDate [a]       = Album a ""
toAlbumAwaitingDate [a,t1,t2] = Album a (t1 <> ": " <> t2)
toAlbumAwaitingDate _         = error "Invalid pattern!"

toDate :: Text -> Date
toDate d = case toUCTTime (T.unpack d) of
  Nothing  -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d %Y" d'

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

main :: IO ()
main = do
  pitchfork <- getPitchforkAlbums
  stereogum <- getStereogumAlbums
  encodeFile "albums.json" stereogum