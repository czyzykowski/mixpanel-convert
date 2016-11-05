{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Mixpanel.ConvertSpec (spec) where

import           Prelude hiding (log)
import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Test.Hspec

import           Mixpanel.Convert


mkFixture "Fixture" [ ''MonadFS ]


spec :: Spec
spec = do
  describe "toLowerKeys" $ do
    it "returns lower cased keys" $ do
      let m = M.fromList [ ("A", String "a"), ("c", String "c") ]
      toLowerKeys m `shouldBe` M.fromList [ ("a", String "a"), ("c", String "c") ]

    it "chooses non-null value if there are duplicated keys when the null is first" $ do
      let m = M.fromList [ ("A", Null), ("a", String "a") ]
      toLowerKeys m `shouldBe` M.fromList [ ("a", String "a") ]

    it "chooses non-null value if there are duplicated keys when the null is second" $ do
      let m = M.fromList [ ("A", String "a"), ("a", Null) ]
      toLowerKeys m `shouldBe` M.fromList [ ("a", String "a") ]


  describe "convert'" $ do
    it "returns nothing for non-existing params" $ do
      convert' M.empty Nothing `shouldBe` Nothing

    it "returns nothing for non-object values" $ do
      convert' M.empty (Just Null) `shouldBe` Nothing

    it "returns nothing for objects without `properties` key" $ do
      let o = object [ "a" .= String "a" ]
      convert' M.empty (Just o) `shouldBe` Nothing

    it "returns nothing for objects without `event` key" $ do
      let o = object [ "properties" .= object [ "a" .= String "b" ] ]
      convert' M.empty (Just o) `shouldBe` Nothing

    it "returns nothing for objects where `properties` is not an object" $ do
      let o = object [ "properties" .= String "a" ]
      convert' M.empty (Just o) `shouldBe` Nothing

    it "returns flattened object with lowercased keys" $ do
      let o = object [ "properties" .= object [ "A" .= String "b"], "event" .= String "c" ]
      convert' M.empty (Just o) `shouldBe` Just (object [ "a" .= String "b", "event" .= String "c" ])

    it "merges with the supplied empty object" $ do
      let o = object [ "properties" .= object [ "A" .= String "b"], "event" .= String "c" ]
      let e = M.fromList [ ("a", Null), ("b", String "d") ]
      let c = object [ "a" .= String "b", "event" .= String "c", "b" .= String "d" ]
      convert' e (Just o) `shouldBe` Just c


  describe "convert" $ do
    it "returns empty string for invalid json" $ do
      convert M.empty "abc" `shouldBe` ""

    it "returns empty string for non-object json" $ do
      convert M.empty "[1]" `shouldBe` ""

    it "returns empty string for invalid objects" $ do
      convert M.empty "{\"a\": 1}" `shouldBe` ""

    it "returns properly encoded and flattened json" $ do
      let s = "{\"event\": \"Event\", \"properties\": {\"A\": 1, \"b\": 2}}"
      let e = "{\"event\":\"Event\",\"a\":1,\"b\":2}"
      convert M.empty s `shouldBe` e


  describe "toKeys" $ do
    it "returns empty list for Nothing" $ do
      toKeys Nothing `shouldBe` []

    it "returns empty list for non-object values" $ do
      toKeys (Just Null) `shouldBe` []

    it "returns empty list for object without `properties` key" $ do
      toKeys (Just (object [ "a" .= String "b" ])) `shouldBe` []

    it "returns list of keys from the properties object" $ do
      let o = object [ "properties" .= object [ "a" .= String "b", "c" .= String "d" ] ]
      toKeys (Just o) `shouldBe` [ "event", "a", "c" ]


  describe "lines'" $ do
    it "splits bytestring into separate lines" $ do
      lines' "a\nb\nc" `shouldBe` [ "a", "b", "c" ]

    it "leaves bytestring without lines intact" $ do
      lines' "abc" `shouldBe` [ "abc" ]

    it "handles empty bytestring" $ do
      lines' "" `shouldBe` []


  describe "unlines'" $ do
    it "combines several stings using new line" $ do
      unlines' [ "a", "b", "c" ] `shouldBe` "a\nb\nc"

    it "leaves singleton string intact" $ do
      unlines' [ "abc" ] `shouldBe` "abc"

    it "handles empty array" $ do
      unlines' [] `shouldBe` ""


  describe "gatherKeys'" $ do
    it "handles empty string" $ do
      gatherKeys' "" `shouldBe` S.empty

    it "handles non-valid json" $ do
      gatherKeys' "abc" `shouldBe` S.empty

    it "handles invalid format" $ do
      gatherKeys' "{\"a\": 1}" `shouldBe` S.empty

    it "collects right keys" $ do
      let s = "{\"properties\": {\"a\": 1, \"b\": 2}}"
      gatherKeys' s `shouldBe` S.fromList [ "a", "b", "event" ]

    it "collects unique keys from multiple objects" $ do
      let s = "{\"properties\": {\"a\": 1, \"b\": 2}}\n{\"properties\": {\"a\": 1, \"c\": 2}}"
      gatherKeys' s `shouldBe` S.fromList [ "a", "b", "c", "event" ]


  describe "gatherKeys" $ do
    it "reads data from the provided path" $ do
      let fixture = def { _readFile = \path -> log path >> return "" }
      let paths = logTestFixture (gatherKeys "test-file.jsonl") fixture
      paths `shouldBe` [ "test-file.jsonl" ]

    it "reads correct data" $ do
      let s = "{\"properties\": {\"a\": 1, \"b\": 2}}"
      let fixture = def { _readFile = \_ -> return s }
      let keys = unTestFixture (gatherKeys "test-file.jsonl") fixture
      keys `shouldBe` S.fromList [ "a", "b", "event" ]


  describe "run" $ do
    it "reads data from the right path" $ do
      let fixture = def { _readFile  = \path -> log path >> return ""
                        , _writeFile = \_ _ -> return ()
                        }
      let paths = logTestFixture (run ("input.jsonl", "output.jsonl")) fixture
      -- double entry into readFile paths list is because both gatherKeys and
      -- read are getting data from the same input file
      paths `shouldBe` [ "input.jsonl", "input.jsonl" ]

    it "writes data to the right path" $ do
      let fixture = def { _readFile  = \_ -> return ""
                        , _writeFile = \path _ -> log path >> return ()
                        }
      let paths = logTestFixture (run ("input.jsonl", "output.jsonl")) fixture
      paths `shouldBe` [ "output.jsonl" ]

    it "processes data properly" $ do
      let s = unlines' [ "{\"properties\": {\"a\": 1, \"b\": 2}, \"event\": \"Event\"}"
                       , "{\"properties\": {\"a\": 1, \"c\": 2}, \"event\": \"Event\"}"
                       ]
      let fixture = def { _readFile  = \_ -> return s
                        , _writeFile = \_ content -> log content >> return ()
                        }
      let contents = logTestFixture (run ("input.jsonl", "output.jsonl")) fixture
      contents `shouldBe` [ "{\"event\":\"Event\",\"a\":1,\"b\":2,\"c\":null}\n{\"event\":\"Event\",\"a\":1,\"b\":null,\"c\":2}" ]
