module Jason.Encode.Class where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeBoolean, encodeChar, encodeCodePoint, encodeEither, encodeForeignObject, encodeIdentity, encodeInt, encodeList, encodeMap, encodeMaybe, encodeNonEmptyArray, encodeNonEmptyList, encodeNonEmpty_Array, encodeNonEmpty_List, encodeNumber, encodeSet, encodeString, encodeTuple, encodeUnit, encodeVoid)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime (Date, DateTime)
import Data.DateTime as DT
import Data.DateTime.ISO (ISO(..))
import Data.Either (Either)
import Data.Formatter.DateTime as DateTimeFormatter
import Data.Identity (Identity)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Days, Hours, Milliseconds, Minutes, Seconds)
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Jason.Common (dateFormatter)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

class JEncode a where
  jencode :: a -> Json

instance jencodeIdentity :: JEncode a => JEncode (Identity a) where
  jencode = encodeIdentity jencode

-- | This encodes Nothing as null
instance jdecodeMaybe :: JEncode a => JEncode (Maybe a) where
  jencode = encodeMaybe jencode

instance jencodeTuple :: (JEncode a, JEncode b) => JEncode (Tuple a b) where
  jencode = encodeTuple jencode jencode

instance jencodeEither :: (JEncode a, JEncode b) => JEncode (Either a b) where
  jencode = encodeEither jencode jencode

instance jencodeUnit :: JEncode Unit where
  jencode = encodeUnit

instance jencodeJBoolean :: JEncode Boolean where
  jencode = encodeBoolean

instance jencodeJNumber :: JEncode Number where
  jencode = encodeNumber

instance jencodeInt :: JEncode Int where
  jencode = encodeInt

instance jencodeJString :: JEncode String where
  jencode = encodeString

instance jjencode :: JEncode Json where
  jencode = identity

instance jencodeCodePoint :: JEncode CodePoint where
  jencode = encodeCodePoint

instance jencodeNonEmpty_Array :: (JEncode a) => JEncode (NonEmpty Array a) where
  jencode = encodeNonEmpty_Array jencode

instance jencodeNonEmptyArray :: (JEncode a) => JEncode (NonEmptyArray a) where
  jencode = encodeNonEmptyArray jencode

instance jencodeNonEmpty_List :: (JEncode a) => JEncode (NonEmpty List a) where
  jencode = encodeNonEmpty_List jencode

instance jencodeNonEmptyList :: (JEncode a) => JEncode (NonEmptyList a) where
  jencode = encodeNonEmptyList jencode

instance jencodeChar :: JEncode Char where
  jencode = encodeChar

instance jencodeArray :: JEncode a => JEncode (Array a) where
  jencode = encodeArray jencode

instance jencodeList :: JEncode a => JEncode (List a) where
  jencode = encodeList jencode

instance jencodeForeignObject :: JEncode a => JEncode (FO.Object a) where
  jencode = encodeForeignObject jencode

instance jencodeSet :: (Ord a, JEncode a) => JEncode (S.Set a) where
  jencode = encodeSet jencode

instance jencodeMap :: (Ord a, JEncode a, JEncode b) => JEncode (M.Map a b) where
  jencode = encodeMap jencode jencode

instance jencodeVoid :: JEncode Void where
  jencode = encodeVoid

-- DateTime
instance jencodeISO :: JEncode ISO where
  jencode = encodeJson

instance jencodeDateTime :: JEncode DateTime where
  jencode = encodeJson <<< ISO

instance jencodeDate :: JEncode Date where
  jencode = encodeJson <<< dateToString
-- Duration.Time
instance jencodeMiilis :: JEncode Milliseconds where
  jencode = jencode <<< unwrap
instance jencodeSeconds :: JEncode Seconds where
  jencode = jencode <<< unwrap

instance jencodeMinutes :: JEncode Minutes where
  jencode = jencode <<< unwrap

instance jencodeHours :: JEncode Hours where
  jencode = jencode <<< unwrap

instance jencodeDays :: JEncode Days where
  jencode = jencode <<< unwrap

instance jencodeRecord ::
  ( JEncodeRL row list
  , RL.RowToList row list
  ) =>
  JEncode ({ | row }) where
  jencode rec = fromObject $ jencodeRL rec (RLProxy :: RLProxy list)

class JEncodeRL (row :: # Type) (list :: RL.RowList) where
  jencodeRL :: Record row -> RLProxy list -> FO.Object Json

instance jencodeRLNil :: JEncodeRL row RL.Nil where
  jencodeRL _ _ = FO.empty
else instance jencodeRLConsMaybe ::
  ( JEncode value
  , JEncodeRL row tail
  , IsSymbol field
  , Row.Cons field (Maybe value) tail' row
  ) =>
  JEncodeRL row (RL.Cons field (Maybe value) tail) where
  jencodeRL row _ = do
    let
      _field = SProxy :: SProxy field

      tail = (jencodeRL row (RLProxy :: RLProxy tail))
    fromMaybe tail
      ( Record.get _field row
          <#> \val ->
              FO.insert
                (reflectSymbol _field)
                (jencode $ val)
                tail
      )
else instance jencodeRLCons ::
  ( JEncode value
  , JEncodeRL row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  ) =>
  JEncodeRL row (RL.Cons field value tail) where
  jencodeRL row _ = do
    let
      _field = SProxy :: SProxy field
    FO.insert
      (reflectSymbol _field)
      (jencode $ Record.get _field row)
      (jencodeRL row (RLProxy :: RLProxy tail))



-- Utils
dateToString :: DT.Date -> String
dateToString d = DateTimeFormatter.format dateFormatter (DT.DateTime (d) (DT.Time hour minute second millisecond))
  where
    hour =  bottom
    minute = bottom
    second = bottom
    millisecond = bottom