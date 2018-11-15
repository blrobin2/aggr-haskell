{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Album ( Album(..)
             , filterAlbums
             , toPartialAlbums
             ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import qualified Data.Text as T
import Dates (Month, Day, getMonthFromDay)
import GHC.Generics (Generic)

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
    errorString s xs = T.unpack $ "Invalid pattern: " <> T.intercalate s xs
