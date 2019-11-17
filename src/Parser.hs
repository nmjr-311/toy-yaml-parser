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
  , withTextT
  )where

import           Control.Applicative
import           Data.Functor.Identity  (Identity)
import qualified Data.Text              as T (Text, pack, strip)
import           Prelude                hiding (sequence)
import qualified Text.Parsec            as P
import           Text.Parsec.Char
import           Text.Parsec.Combinator (eof, many1, manyTill)

import           Debug.Trace
import           Types

-- indent size
newtype ISize = ISize Int deriving (Eq, Show, Ord)

type ParserT m a = P.ParsecT T.Text ISize m a
type YamlParserT m = ParserT m Yaml

validChar :: Monad m => ParserT m  Char
validChar = satisfy (`notElem` (":-\n" :: String))

indent :: Monad m => ParserT m ISize
indent = do
  n <- fmap (ISize . length) (many space)
  P.setState n
  return n

withIndent :: Monad m => ParserT m a -> ParserT m a
withIndent p = do
  before <- P.getState
  trace' ("Before " ++ show before)
  n <- indent
  trace' ("After " ++ show n)
  if n < before
    then P.setState before >> fail "indent mismatch"
    else p
  where
    trace' n = do
      pos <- P.getPosition
      trace ("current=" ++ show pos ++ ", indent=" ++ n) $ pure ()

colon :: Monad m => ParserT m ()
colon = char ':' *> (spaces <|> newline')

hyphen :: Monad m => ParserT m ()
hyphen = withIndent $ char '-' *> (spaces <|> newline')

newline' :: Monad m => ParserT m ()
newline' = () <$ (newline <|> char '\n' <|> endOfLine)

yaml :: Monad m => YamlParserT m
yaml = seq' <|> map' <|> lit'
  where
    lit' = spaces *> lit
    seq' = P.try $ withIndent sequence
    map' = P.try $ withIndent mapping

lit :: Monad m => YamlParserT m
lit = P.try litNum <|> litStr

litStr :: Monad m => YamlParserT m
litStr = LitStr <$> (P.try quote <|> normal)
  where
    normal = T.strip . T.pack <$> manyTill validChar (newline' <|> eof)
    quote = T.pack <$> (char '"' *> manyTill anyChar (char '"') <* (newline' <|> eof))

litNum :: Monad m => YamlParserT m
litNum = LitNum . read <$> manyTill digit (newline' <|> eof)

key :: Monad m => ParserT m Key
key = Key . T.strip . T.pack <$> manyTill validChar colon

pair :: Monad m => ParserT m (Key, Yaml)
pair = liftA2 (,) key yaml

mapping :: Monad m => YamlParserT m
mapping = Mapping <$> many1 pair

sequence :: Monad m => YamlParserT m
sequence = Sequence <$> many1 (hyphen *> noIndentYaml)
  where
    noIndentYaml = P.try sequence <|> P.try mapping <|> spaces *> lit

withText :: ParserT Identity a -> T.Text -> Either P.ParseError a
withText p = P.runParser p (ISize 0) "text"

withTextT :: Monad m => ParserT m a -> T.Text -> m (Either P.ParseError a)
withTextT p = P.runParserT p (ISize 0) "text"
