{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Data.Time.JSON
    (DateTime(..))
    where

import Data.Time
import System.Time
import System.Locale           (defaultTimeLocale)
import Text.JSON

-- | A type for defining a UTCTime JSON instance.
newtype DateTime = DateTime { unDateTime :: UTCTime }
  deriving (Eq)
  
instance Show DateTime where
  show (DateTime x) = formatUTC x

instance JSON DateTime where
  readJSON x = case x of 
    JSString s -> case parseTime defaultTimeLocale timeFormat (fromJSString s) of
      Just t -> Ok $ DateTime t
      Nothing -> Error $ "Unable to parse time: " ++ fromJSString s
    _ -> Error $ "Unable to parse time from JSON value: " ++ show x
  showJSON = showJSON . formatUTC . unDateTime
    
formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale timeFormat

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S %Z"
