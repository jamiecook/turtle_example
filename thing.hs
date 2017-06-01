#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}  --
{-# LANGUAGE RecordWildCards #-}
                                    --
import Turtle                       --
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import Data.Csv
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Vector (Vector)
import qualified Data.Vector as Vector


data ItemType
  = Country
  | Other Text
  deriving (Eq, Show)

data Item =
  Item
    { itemName :: Text
    , itemLink :: Text
    , itemType :: ItemType
    }
  deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Item"
      <*> m .: "Link"
      <*> m .: "Type"

instance FromField ItemType where
  parseField "International Country" =
    pure Country

  parseField otherType =
    Other <$> parseField otherType

catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show

decodeItems
  :: ByteString
  -> Either String (Vector Item)
decodeItems =
  fmap snd . decodeByName

decodeItemsFromFile
  :: Prelude.FilePath
  -> IO (Either String (Vector Item))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeItems

main = do
  echo "Hello, world!"         -- echo Hello, world!jkk
  foo <- decodeItemsFromFile "items.csv"
  case foo of
    Left a -> print a
    Right b -> print b
