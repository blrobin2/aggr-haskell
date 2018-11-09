{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>))
import           Data.Aeson (ToJSON, defaultOptions, encodeFile, genericToEncoding, toEncoding)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics (Generic)
import           Network.HTTP.Client (parseRequest)
import           Network.HTTP.Simple (Request, httpLBS, httpSink, getResponseBody)
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

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let albumsAwaitingDate = toAlbumsAwaitingDate $ getArtistAndTitle cursor
  let date = getReleaseDate cursor
  let albums = zipWith addDateToAlbum albumsAwaitingDate date
  scores <- getReviewScore cursor
  let pairs = zip scores albums
  let filtered = map snd $ filter (\(score, at) -> (read . T.unpack $ score :: Double) >= 8.0) pairs
  return filtered

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = do
  doc <- httpSink url $ const sinkDoc
  return $ fromDocument doc

getArtistAndTitle :: Cursor -> [Text]
getArtistAndTitle cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDate :: Cursor -> [Text]
getReleaseDate cursor = map toDate dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

getReviewScore :: Cursor -> IO [Text]
getReviewScore cursor = do
  let link = getReviewLink cursor
  traverse score link
  where
    score l = do
      request <- parseRequest l
      reviewCursor <- getXmlCursor $ request
      return $ T.concat $ reviewCursor $// element "span" >=> attributeIs "class" "score" &// content

getReviewLink :: Cursor -> [String]
getReviewLink cursor = filtered
  where
    elem = cursor $// element "item" &// content
    filtered = filter isLink (map T.unpack elem)

isLink :: String -> Bool
isLink str = str!!0 == 'h' && str!!1 == 't' && str!!2 == 't' && str!!3 == 'p'

toAlbumsAwaitingDate :: [Text] -> [(Date -> Album)]
toAlbumsAwaitingDate = map (toAlbumAwaitingDate . map T.strip . T.splitOn ":")

toAlbumAwaitingDate :: [Text] -> (Date -> Album)
toAlbumAwaitingDate [a, t]       = Album a t
toAlbumAwaitingDate [a]          = Album a ""
toAlbumAwaitingDate (a:t1:t2:[]) = Album a (t1 <> ": " <> t2)
toAlbumAwaitingDate _            = error "Invalid pattern!"

addDateToAlbum :: (Date -> Album) -> Date -> Album
addDateToAlbum a d = a d

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

toDate :: Text -> Date
toDate d = case toUCTTime (T.unpack d) of
  Nothing  -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d %Y" d'

main :: IO ()
main = do
  albums <- getPitchforkAlbums
  encodeFile "albums.json" albums