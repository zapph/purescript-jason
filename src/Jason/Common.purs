module Jason.Common where

import Prelude

import Data.Either (fromRight)
import Data.Formatter.DateTime as DateTimeFormatter
import Partial.Unsafe (unsafePartial)



dateFormatter :: DateTimeFormatter.Formatter
dateFormatter = unsafePartial $ fromRight $ DateTimeFormatter.parseFormatString "YYYY-MM-DD"
