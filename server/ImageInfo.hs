module ImageInfo (imageInfo) where

import Common
import Types

import System.Process (readProcessWithExitCode)

import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
--import Control.Monad.Combinators


defaultInfo :: Dim -> DocInfo
defaultInfo dim = DocInfo
  { modified = Nothing
  , included = False
  , imageSize = dim
  }

type Parser = Parsec Void String

parseIdentify :: Parser (FilePath, String, Dim)
parseIdentify = do
  filename <- parseFilename
  space
  code <- fileCode
  space
  dim <- parseDim
  many anyChar
  return (filename, code, dim)


parseFilename :: Parser String
parseFilename = takeWhile1P Nothing (not . isSpace) <?> "word"

fileCode :: Parser String
fileCode = some letterChar <?> "file code"

parseDim :: Parser Dim
parseDim = do
  w <- decimal
  char 'x'
  h <- decimal
  return (w, h)


imageInfo :: FilePath -> IO (Maybe DocInfo)
imageInfo filename = do
  (exit, out, _) <- readProcessWithExitCode "identify" [filename] ""
  return $ case exit of
    ExitSuccess -> toInfo <$> parseMaybe parseIdentify out
    _           -> Nothing

    where
      toInfo (_, _, dim) = defaultInfo dim
