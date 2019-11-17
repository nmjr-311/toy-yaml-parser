{-# LANGUAGE OverloadedStrings #-}
module ParserSpec
  ( spec
  ) where

import qualified Data.Text  as T (concat)
import           Parser
import           Prelude    hiding (sequence)
import           Test.Hspec
import           Types

-- all specs
spec :: Spec
spec =
  litStrSpec *> litNumSpec
  *> litSpec *> keySpec
  *> pairSpec *> mappingSpec
  *> sequenceSpec *> exampleSpec

litSpec :: Spec
litSpec =
  describe "lit" $ do
  it "should parse integer" $
    withText lit "123" `shouldBe` Right (LitNum 123)

  it "should parse string" $
    withText lit "abc" `shouldBe` Right (LitStr "abc")

  it "should return LitStr when entirely text is string" $
    withText lit "123abc" `shouldBe` Right (LitStr "123abc")

litStrSpec :: Spec
litStrSpec =
  describe "litStr" $ do
    it "should parse alphaNums" $
      withText litStr "abcd2345efg" `shouldBe` Right (LitStr "abcd2345efg")

    it "should parse string containing space" $
      withText litStr "abcd2  345efg" `shouldBe` Right (LitStr "abcd2  345efg")

    it "should parse string until newline" $
      withText litStr "abc \n def" `shouldBe` Right (LitStr "abc")

    it "should return trimmed string" $
      withText litStr " abc " `shouldBe` Right (LitStr "abc")

    it "should parse double quotation style" $
      withText litStr "\" a:bc\ndef\n   gef \"" `shouldBe` Right (LitStr " a:bc\ndef\n   gef ")

litNumSpec :: Spec
litNumSpec =
  describe "litNum" $
    it "should parse nums" $
      withText litNum "2345" `shouldBe` Right (LitNum 2345)

keySpec :: Spec
keySpec =
  describe "key" $ do
    it "should parse string until reaches colon" $
      withText key "key : value" `shouldBe` Right (Key "key")

    it "should parse string even if text contains numbers" $
      withText key "ke123y : value" `shouldBe` Right (Key "ke123y")

    it "should parse string even if text contains spaces" $
      withText key "ke  y : value" `shouldBe` Right (Key "ke  y")

pairSpec :: Spec
pairSpec =
  describe "pair" $ do
  it "should parse (key, string) pair separated by colon" $
    withText pair "key: 1value" `shouldBe` Right (Key "key", LitStr "1value")

  it "should parse (key, integer) pair separated by colon" $
    withText pair "key: 123" `shouldBe` Right (Key "key", LitNum 123)

  it "should parse (key, string) pair even if string contains spaces" $
    withText pair "key  :   ab  cd 12 df  " `shouldBe` Right (Key "key", LitStr "ab  cd 12 df")

mappingSpec :: Spec
mappingSpec =
  describe "mapping" $ do
  it "should parse singleton (key: value) pairg" $
    withText mapping "key1: value1" `shouldBe` Right (Mapping [(Key "key1", LitStr "value1")])

  it "should parse (key: value) pairs" $
    withText mapping "key1: value1 \nkey2: 987" `shouldBe`
    Right (Mapping [(Key "key1", LitStr "value1"), (Key "key2", LitNum 987)])

  it "should parse (key: sequence) pair" $
    withText mapping "key:\n  - value1\n  - 111\n  - 2aa\n" `shouldBe`
    Right (Mapping [(Key "key", Sequence [LitStr "value1", LitNum 111, LitStr "2aa"])])

sequenceSpec :: Spec
sequenceSpec =
  describe "sequence" $ do
  it "should parse singleton sequence" $
    withText sequence "- a" `shouldBe` Right (Sequence [LitStr "a"])

  it "should parse sequence of literals" $
    withText sequence "- a\n- 1" `shouldBe` Right (Sequence [LitStr "a", LitNum 1])

exampleSpec :: Spec
exampleSpec =
  describe "examples of valid YAMLs" $ do
    it "sequence of scalars" $
      withText yaml "- aaa\n- bbb\n- 123"
      `shouldBe` Right (Sequence [LitStr "aaa", LitStr "bbb", LitNum 123])
    it "mapping scalars to sequences" $
      withText yaml
      (T.concat [ "a:\n  - b\n  - 12\n  - c\n"
                , "d:\n  - e\n  - 34\n  - f"])
      `shouldBe`
      Right (Mapping [ ( Key "a"
                       , Sequence [ LitStr "b"
                                  , LitNum 12
                                  , LitStr "c"])
                     , ( Key "d"
                       , Sequence [ LitStr "e"
                                  , LitNum 34
                                  , LitStr "f"])])
    it "sequence of mappings" $
      withText yaml
      (T.concat [ "-\n  a: b1\n  c:  11\n"
                , "-\n  a: b2\n  c:  22"])
      `shouldBe`
      Right (Sequence [ Mapping [ (Key "a", LitStr "b1")
                                , (Key "c", LitNum 11) ]
                      , Mapping [ (Key "a", LitStr "b2")
                                , (Key "c", LitNum 22) ] ])
    it "complex structure" $
      withText yaml
      (T.concat [ "number: 1234\n"
                , "string: string\n"
                , "mapOfMap: \n"
                , "  attr1: attr1\n"
                , "  attr2: attr2\n"
                , "  map:\n"
                , "    quotes:\n"
                , "      \"abc\nspace \""
                , "    attr1 : abc\n"
                , "seqMap:\n"
                , "  - elem1: 123\n"
                , "    elem2: abc\n"
                , "  - elem1: 456\n"
                , "    elem2: def\n"
                , "fin: \"122\""]) `shouldBe`
      Right (Mapping [ (Key "number", LitNum 1234)
                     , (Key "string", LitStr "string")
                     , ( Key "mapOfMap"
                       , Mapping [ (Key "attr1", LitStr "attr1")
                                 , (Key "attr2", LitStr "attr2")
                                 , (Key "map", Mapping [ ( Key "quotes"
                                                         , LitStr "abc\nspace ")
                                                         , (Key "attr1", LitStr "abc")])])
                   , ( Key "seqMap"
                     , Sequence [ Mapping [ (Key "elem1", LitNum 123)
                                          , (Key "elem2", LitStr "abc")]
                                , Mapping [ (Key "elem1", LitNum 456)
                                          , (Key "elem2", LitStr "def")]])
                   , (Key "fin", LitStr "122")])
