module Jason.Decode.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeBoolean, decodeCodePoint, decodeEither, decodeForeignObject, decodeIdentity, decodeInt, decodeList, decodeMap, decodeMaybe, decodeNonEmptyArray, decodeNonEmptyList, decodeNonEmpty_Array, decodeNonEmpty_List, decodeNull, decodeNumber, decodeSet, decodeString, decodeTuple, decodeVoid, getFieldOptional')
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (Date, DateTime)
import Data.DateTime as DT
import Data.DateTime.ISO (ISO, unwrapISO)
import Data.Either (Either(..), either, hush, note)
import Data.Formatter.DateTime as DateTimeFormatter
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..))
import Data.Tuple (Tuple)
import Foreign.Object as FO
import Jason.Common (dateFormatter)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as RecordB
import Type.Data.RowList (RLProxy(..))

-- | Use this for decoding JSON
jdecode :: forall a. JDecode a => Json -> Either JsonDecodeError a
jdecode json =
  jdecode' json
    <|> ( do
          obj <- note (TypeMismatch "Object") $ toObject json
          j <-
            (note (AtKey "value" MissingValue) (FO.lookup "value" obj))
              <|> (note (AtKey "val" MissingValue) (FO.lookup "val" obj))
          jdecode' j
      )

-- Class implementations
class JDecode a where
  jdecode' :: Json -> Either JsonDecodeError a

instance jdecodeIdentity :: JDecode a => JDecode (Identity a) where
  jdecode' = decodeIdentity jdecode

instance jdecodeMaybe :: JDecode a => JDecode (Maybe a) where
  jdecode' j = decodeMaybe jdecode j

instance jdecodeTuple :: (JDecode a, JDecode b) => JDecode (Tuple a b) where
  jdecode' = decodeTuple jdecode jdecode

instance decodeJsonEither :: (JDecode a, JDecode b) => JDecode (Either a b) where
  jdecode' = decodeEither jdecode jdecode

instance jdecodeNull :: JDecode Unit where
  jdecode' = decodeNull

instance jdecodeBoolean :: JDecode Boolean where
  jdecode' j = decodeBoolean j <|> (pure $ isTruthy j)

instance jdecodeNumber :: JDecode Number where
  jdecode' = decodeNumber

instance jdecodeInt :: JDecode Int where
  jdecode' = decodeInt

instance jdecodeString :: JDecode String where
  jdecode' = decodeString

instance jdecodeJson :: JDecode Json where
  jdecode' = Right

instance jdecodeCodePoint :: JDecode CodePoint where
  jdecode' = decodeCodePoint

instance jdecodeNonEmpty_Array :: (JDecode a) => JDecode (NonEmpty Array a) where
  jdecode' j = decodeNonEmpty_Array jdecode j <|> asSingle j

instance jdecodeNonEmptyArray :: (JDecode a) => JDecode (NonEmptyArray a) where
  jdecode' j = decodeNonEmptyArray jdecode j <|> asSingle j

instance jdecodeNonEmpty_List :: (JDecode a) => JDecode (NonEmpty List a) where
  jdecode' j = decodeNonEmpty_List jdecode j <|> asSingle j

instance jdecodeNonEmptyList :: (JDecode a) => JDecode (NonEmptyList a) where
  jdecode' j = decodeNonEmptyList jdecode j <|> asSingle j

instance decodeForeignObject :: JDecode a => JDecode (FO.Object a) where
  jdecode' = decodeForeignObject jdecode

instance decodeArray :: JDecode a => JDecode (Array a) where
  jdecode' j = decodeArray jdecode j <|> asSingle j

instance decodeList :: JDecode a => JDecode (List a) where
  jdecode' j = decodeList jdecode j <|> asSingle j

instance decodeSet :: (Ord a, JDecode a) => JDecode (S.Set a) where
  jdecode' j = decodeSet jdecode j <|> asSingle j

instance decodeMap :: (Ord a, JDecode a, JDecode b) => JDecode (M.Map a b) where
  jdecode' = decodeMap jdecode jdecode

instance decodeVoid :: JDecode Void where
  jdecode' = decodeVoid

-- DateTime
instance decodeISO :: JDecode ISO where
  jdecode' = decodeJson

instance decodeDateTime :: JDecode DateTime where
  jdecode' = map unwrapISO <<< jdecode

instance decodeDate :: JDecode Date where
  jdecode' = decodeFormatted dateFromString

-- Duration.Time
instance decodeDMilliseconds :: JDecode Milliseconds where
  jdecode' = map Milliseconds <<< jdecode

instance decodeDSeconds :: JDecode Seconds where
  jdecode' = map Seconds <<< jdecode

instance decodeDMinutes :: JDecode Minutes where
  jdecode' = map Minutes <<< jdecode

instance decodeDHours :: JDecode Hours where
  jdecode' = map Hours <<< jdecode

instance decodeDDays :: JDecode Days where
  jdecode' = map Days <<< jdecode

-- Record
instance jdecodeRecord ::
  ( RL.RowToList r rl
  , JDecodeRL () r rl
  ) =>
  JDecode (Record r) where
  jdecode' j =
    let
      jdecodeRL_ obj = jdecodeRL obj (RLProxy :: _ rl) <#> \rb -> RecordB.build rb {}
    in
      ( (note (TypeMismatch "Object") $ toObject j)
          >>= jdecodeRL_
      )
        <|> ( (note (TypeMismatch "{ value :: Object }") $ toObject $ encodeJson { value: j })
              >>= jdecodeRL_
          )
        <|> ( (note (TypeMismatch "{ val :: Object }") $ toObject $ encodeJson { val: j })
              >>= jdecodeRL_
          )

class JDecodeRL (from :: # Type) (to :: # Type) (list :: RL.RowList) | list -> from to where
  jdecodeRL :: FO.Object Json -> RLProxy list -> Either JsonDecodeError (RecordB.Builder { | from } { | to })

instance jDecodeRLNil :: JDecodeRL () () RL.Nil where
  jdecodeRL _ _ = Right identity
else instance jDecodeRLConsMaybe ::
  ( JDecode value
  , JDecodeRL from mid tail
  , IsSymbol field
  , Row.Cons field (Maybe value) mid to
  , Row.Lacks field mid
  ) =>
  JDecodeRL from to (RL.Cons field (Maybe value) tail) where
  jdecodeRL object _ = do
    val <- (getFieldOptional' jdecode object fieldName) <|> (Right Nothing)
    rest <- jdecodeRL object (RLProxy :: _ tail)
    pure $ RecordB.insert label val <<< rest
    where
    label :: SProxy field
    label = SProxy

    fieldName = reflectSymbol label
else instance jDecodeRLCons ::
  ( JDecode value
  , JDecodeRL from mid tail
  , IsSymbol field
  , Row.Cons field value mid to
  , Row.Lacks field mid
  ) =>
  JDecodeRL from to (RL.Cons field value tail) where
  jdecodeRL object _ = do
    json <- note (AtKey fieldName MissingValue) $ FO.lookup fieldName object
    val <- jdecode json
    rest <- jdecodeRL object (RLProxy :: _ tail)
    pure $ RecordB.insert label val <<< rest
    where
    label :: SProxy field
    label = SProxy

    fieldName = reflectSymbol label

isTruthy :: Json -> Boolean
isTruthy j = testStr || testArr || testObj || testNull || testInt
  where
  -- [] -> false
  testArr = e $ (decodeArray jdecode' j :: _ _ (Array Json)) <#> not Arr.null

  -- {} -> false
  testObj = case toObject j <#> not FO.isEmpty of
    Just bool -> bool
    Nothing -> false

  -- null -> false
  testNull = e $ decodeNull j <#> not eq unit

  -- 0 -> false
  testInt = e $ decodeInt j <#> not eq 0

  -- "" -> false
  testStr =
    e $ decodeString j
      <#> not eq ""

  e = either (const false) (identity)

asSingle :: forall f a. Single f => JDecode a => Json -> Either JsonDecodeError (f a)
asSingle = map single <<< jdecode'

class Single f where
  single :: forall a. a -> f a

instance arrSingleton :: Single (Array) where
  single = Arr.singleton

instance listSingle :: Single (List) where
  single = List.singleton

instance nonEmptyArrSingle :: Single (NonEmptyArray) where
  single = NEA.singleton

instance nonEmptyListSingle :: Single (NonEmptyList) where
  single = NEL.singleton

instance setSingle :: Single (S.Set) where
  single = S.singleton

instance nonEmptySingle :: Plus f => Single (NonEmpty f) where
  single x = x :| empty



-- Utils
dateFromString :: String -> Maybe DT.Date
dateFromString s =
  map (DT.date) <<< hush $ DateTimeFormatter.unformat dateFormatter s

decodeFormatted ::
  forall a b.
  JDecode a =>
  (a -> Maybe b) ->
  Json ->
  Either JsonDecodeError b
decodeFormatted f j = do
  s <- jdecode j
  note (UnexpectedValue j) (f s)
