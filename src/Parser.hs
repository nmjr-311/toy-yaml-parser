module Parser
  ( yaml
  , lit
  , litStr
  , litNum
  , key
  , pair
  , mapping
  , sequence
  , withText
  )where

import           Control.Applicative
import           Data.Functor.Identity  (Identity)
import qualified Data.Text              as T (Text, pack, strip)
import           Prelude                hiding (sequence)
import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator (eof, many1, manyTill)

import           Types

-- indent size
newtype ISize = ISize Int deriving (Eq, Show, Ord)

type ParserT m a = P.ParsecT T.Text ISize m a
type Parser a = ParserT Identity a
type YamlParserT m = ParserT m Yaml
type YamlParser = YamlParserT Identity

validChar :: Parser Char
validChar = satisfy (`notElem` (":-\n" :: String))

indent :: Parser ()
indent = fmap length (many space) >>= P.setState . ISize

colon :: Parser ()
colon = char ':' *> (spaces <|> newline')

hyphen :: Parser ()
hyphen = char '-' *> (spaces <|> newline')

newline' :: Parser ()
newline' = () <$ (newline <|> char '\n' <|> endOfLine) *> indent

yaml :: YamlParser
yaml = P.try sequence <|> P.try mapping <|> lit

lit :: YamlParser
lit = P.try litNum <|> litStr

litStr :: YamlParser
litStr = LitStr <$> (P.try quote <|> normal)
  where
    normal = T.strip . T.pack <$> manyTill validChar (newline' <|> eof)
    quote = T.pack <$> (char '"' *> manyTill anyChar (char '"') <* (newline' <|> eof))

litNum :: YamlParser
litNum = LitNum . read <$> manyTill digit (newline' <|> eof)

key :: Parser Key
key = Key . T.strip . T.pack <$> manyTill validChar colon

pair :: Parser (Key, Yaml)
pair = liftA2 (,) key yaml

mapping :: YamlParser
mapping = Mapping <$> many1 pair

sequence :: YamlParser
sequence = Sequence <$> many1 (hyphen *> yaml)

withText :: Parser a -> T.Text -> Either P.ParseError a
withText p = P.runParser p (ISize 0) "text"
