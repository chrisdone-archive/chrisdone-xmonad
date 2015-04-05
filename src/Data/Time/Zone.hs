-- | Get time zones.

module Data.Time.Zone where

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

-- | Get the time zone by name.
getZone :: String -> Maybe TimeZone
getZone zone =
  case parseTime defaultTimeLocale "%F%T%Z" ("2000-01-0100:00:00" ++ zone) of
    Just (ZonedTime _ timeZone) -> Just timeZone
    _ ->
      Nothing
