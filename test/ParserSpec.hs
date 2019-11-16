module ParserSpec
  ( spec
  ) where

import           Parser
import           Test.Hspec
import           Types

-- all specs
spec :: Spec
spec = litStrSpec *> litNumSpec *> litSpec *> keySpec *> pairSpec

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

    it "should fail when key contains spaces" $
      withText key "ke y : value" `shouldSatisfy` parseFail

pairSpec :: Spec
pairSpec =
  describe "pair" $ do
  it "should parse (key, string) pair separated by colon" $
    withText pair "key: 1value" `shouldBe` Right (Key "key", LitStr "1value")

  it "should parse (key, integer) pair separated by colon" $
    withText pair "key: 123" `shouldBe` Right (Key "key", LitNum 123)

  it "should parse (key, string) pair even if string contains spaces" $
    withText pair "key  :   ab  cd 12 df  " `shouldBe` Right (Key "key", LitStr "ab  cd 12 df")

parseFail :: Either l r -> Bool
parseFail = \case
  Left _ -> True
  Right _ -> False
