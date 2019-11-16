module Types where

import Data.Text (Text)

data Yaml where
  LitStr :: Text -> Yaml
  LitNum :: Int -> Yaml
  Sequence :: [Yaml] -> Yaml
  Mapping :: [(Key, Yaml)] -> Yaml
  deriving (Show, Eq)

newtype Key = Key Text deriving (Show, Eq)
