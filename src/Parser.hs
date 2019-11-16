module Parser
  ( lit
  , litStr
  , litNum
  , key
  , pair
  , withText
  )where

import           Control.Applicative
import qualified Data.Text              as T (Text, pack, strip)
import qualified Text.Parsec            as P (ParseError, runParser, try)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import qualified Text.Parsec.Text       as T

import           Types

colon :: T.Parser ()
colon = spaces *> char ':' *> spaces

newline' :: T.Parser ()
newline' = newline *> pure ()

yaml :: T.Parser Yaml
yaml = lit

lit :: T.Parser Yaml
lit = P.try litNum <|> litStr

litStr :: T.Parser Yaml
litStr = LitStr . T.strip . T.pack <$> manyTill (satisfy (/= ':')) (newline' <|> eof)

litNum :: T.Parser Yaml
litNum = LitNum . read <$> manyTill digit (newline' <|> eof)

key :: T.Parser Key
key = Key . T.strip . T.pack <$> manyTill alphaNum colon

pair :: T.Parser (Key, Yaml)
pair = liftA2 (,) key yaml

withText :: T.Parser a -> T.Text -> Either P.ParseError a
withText p t = P.runParser p () "text" t
