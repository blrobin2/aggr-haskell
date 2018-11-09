{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson (ToJSON, defaultOptions, encodeFile, genericToEncoding, toEncoding)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics (Generic)
import           Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import           Text.XML (def, parseLBS_)
import           Text.XML.Cursor (Cursor, content, element, fromDocument, ($/), ($//), (&/), (&//))

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Text
  } deriving (Generic, Eq, Show)

instance ToJSON Album where
  toEncoding = genericToEncoding defaultOptions

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let albumsAwaitingDate = toAlbumsAwaitingDate $ geArtistAndTitle cursor
  let date = getReleaseDate cursor
  return $ zipWith addDateToAlbum albumsAwaitingDate date

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = do
  response <- httpLBS url
  let xmlDocument = parseLBS_ def (getResponseBody response)
  return $ fromDocument xmlDocument

geArtistAndTitle :: Cursor -> [Text]
geArtistAndTitle cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDate :: Cursor -> [Text]
getReleaseDate cursor = map toDate dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

toAlbumsAwaitingDate :: [Text] -> [(Text -> Album)]
toAlbumsAwaitingDate = map (toAlbumAwaitingDate . map T.strip . T.splitOn ":")

toAlbumAwaitingDate :: [Text] -> (Text -> Album)
toAlbumAwaitingDate [a, t]       = Album a t
toAlbumAwaitingDate [a]          = Album a ""
toAlbumAwaitingDate (a:t1:t2:[]) = Album a (t1 <> ": " <> t2)
toAlbumAwaitingDate _            = error "Invalid pattern!"

addDateToAlbum :: (Text -> Album) -> Text -> Album
addDateToAlbum a d = a d

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

toDate :: Text -> Text
toDate d = case toUCTTime (T.unpack d) of
  Nothing  -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d %Y" d'

main :: IO ()
main = do
  albums <- getPitchforkAlbums
  encodeFile "albums.json" albums