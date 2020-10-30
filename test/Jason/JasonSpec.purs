module Jason.JasonSpec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..))
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Now (nowDate, nowDateTime)
import Foreign.Object (Object)
import Foreign.Object as FO
import Jason.Decode.Class (class JDecode, jdecode, single)
import Jason.Encode.Class (class JEncode, jencode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

jasonSpec :: Spec Unit
jasonSpec = do
  describe "JasonSpec" do
    it "should encode and decode Duration.Time components as numbers" do
      shouldEncodeAndDecode (Minutes 10.0)
      shouldEncodeAndDecode (Hours 10.0)
      shouldEncodeAndDecode (Milliseconds 10.0)
      shouldEncodeAndDecode (Days 10.0)
      shouldEncodeAndDecode (Seconds 10.0)
    it "should encode and decode Date.DateTime as ISO format" do
      date <- liftEffect $ nowDate
      shouldEncodeAndDecode date
    it "should encode and decode Date.Date" do
      dt <- liftEffect $ nowDateTime
      shouldEncodeAndDecode dt
    describe "jencode" do
      it "should skip row if value is Nothing in encoding" do
        let
          jval = jencode { value: Nothing :: _ Int }

          jobj = jdecode jval :: _ _ (Object Int)
        (jobj <#> FO.lookup "value") `shouldEqual` Right Nothing
    describe "jdecode" do
      it "should try { value: a } and { val: a } to decode a" do
        let
          jval' = encodeJson { value: 1 }

          jval'' = encodeJson { val: 1 }

          jvalM = encodeJson { value: 1 }

          jvalArr = encodeJson { value: [ 1 ] }

          jsum = encodeJson { value: Foo }
        jdecode jval' `shouldEqual` (Right 1)
        jdecode jval'' `shouldEqual` (Right 1)
        jdecode jvalM `shouldEqual` (Right (Just 1))
        jdecode jvalArr `shouldEqual` (Right [ 1 ])
        jdecode jsum `shouldEqual` (Right Foo)
      it "should decode { value : a } from a " do
        let
          jval' = encodeJson "AuthToken"
        jdecode jval' `shouldEqual` (Right { value: "AuthToken" })
        jdecode jval' `shouldEqual` (Right { val: "AuthToken" })
    describe "Record" do
      it "should decode missing keys in Json as Nothing" do
        let
          emptyJ :: Json
          emptyJ = encodeJson {}

          mixedJ :: Json
          mixedJ = encodeJson { name: Just "Sean", age: 1 }
        jdecode emptyJ
          `shouldEqual`
            ( Right
                { name: Nothing :: _ String
                , age: Nothing :: _ Int
                }
            )
        jdecode mixedJ
          `shouldEqual`
            ( Right
                { name: Just "Sean" :: _ String
                , age: Just 1 :: _ Int
                , address: Nothing :: _ String
                }
            )
      it "should decode in inner object's { value : a } and { val: a } if decoding failed in exact key" do
        let
          jvalue = encodeJson { test: { value: 1, currency: "PHP" } }

          jval = encodeJson { test: { val: 1, currency: "PHP" } }

          jsumVal = encodeJson { test: { val: Foo, currency: "PHP" } }

          jsumValue = encodeJson { test: { val: Foo, currency: "PHP" } }
        jdecode jvalue `shouldEqual` (Right { test: 1 })
        jdecode jval `shouldEqual` (Right { test: 1 })
        jdecode jsumVal `shouldEqual` (Right { test: Foo })
        jdecode jsumValue `shouldEqual` (Right { test: Just Foo })
    describe "Boolean" do
      it "should decode truthy values as true and falsy values as false" do
        let
          randStr :: Json
          randStr = encodeJson "rand"

          arr :: Json
          arr = encodeJson ([ 1 ] :: Array Int)

          obj :: Json
          obj = encodeJson { test: "hi " }

          -- Empty things
          emptyStr :: Json
          emptyStr = encodeJson ""

          null :: Json
          null = encodeJson unit

          emptyObj :: Json
          emptyObj = encodeJson {}

          emptyArr :: Json
          emptyArr = encodeJson ([] :: _ Int)

          zero :: Json
          zero = encodeJson 0

          truthyValues = [ arr, obj, randStr ]

          falsyValues = [ emptyStr, null, emptyObj, emptyArr, zero ]
        for_ truthyValues
          ( \j ->
              jdecode j `shouldEqual` (Right true)
          )
        for_ falsyValues
          ( \j ->
              jdecode j `shouldEqual` (Right false)
          )
    describe "Collection types" do
      it "should decode an f a from a" do
        let
          d = 1

          json = encodeJson 1
        jdecode json `shouldEqual` (Right (single d :: S.Set Int))
        jdecode json `shouldEqual` (Right (single d :: Array Int))
        jdecode json `shouldEqual` (Right (single d :: NonEmptyArray Int))
        jdecode json `shouldEqual` (Right (single d :: NonEmptyList Int))
        jdecode json `shouldEqual` (Right (single d :: S.Set Int))

---- Generic Sum Types ----
data SumTest
  = Foo
  | Bar
  | Baz

derive instance genericSumTest :: Generic SumTest _

derive instance eqSumTest :: Eq SumTest

instance showSumTest :: Show SumTest where
  show = genericShow

instance encodeJsonSumTest :: EncodeJson SumTest where
  encodeJson = genericEncodeJson

instance jdecodeSumTest :: JDecode SumTest where
  jdecode' = genericDecodeJson

-- Utils
shouldEncodeAndDecode ::
  forall a m.
  MonadThrow Error m =>
  Show (Either JsonDecodeError a) =>
  Eq (Either JsonDecodeError a) =>
  JEncode a => JDecode a =>
  a -> m Unit
shouldEncodeAndDecode a =
  let
    json = jencode a :: Json
  in
    jdecode json `shouldEqual` Right a
