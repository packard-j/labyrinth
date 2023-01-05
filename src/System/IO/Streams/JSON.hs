module System.IO.Streams.JSON (parseJSONStream) where
import Data.Aeson.Types (Value)
import Data.Aeson.Parser (value)
import Data.ByteString
import Data.Attoparsec.ByteString (Parser, endOfInput)
import Data.Attoparsec.ByteString.Char8 (skipSpace)
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString (parserToInputStream)
import Control.Applicative

-- | Parse a stream of JSON values
parseJSONStream :: InputStream ByteString -> IO (InputStream Value)
parseJSONStream = parserToInputStream parseValues

-- | A parser that parses JSON values preceeded by whitespace until the end of input
parseValues :: Parser (Maybe Value)
parseValues = (spaceThen endOfInput >> pure Nothing) <|> (Just <$> spaceThen value) where
  spaceThen parser = skipSpace *> parser
