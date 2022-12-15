module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Control.Monad
import Data.Void

-- Тип парсера (аппликативный функтор):
--
-- type Parser a  =  String -> Either Error [(a, String)]

type Parser = Parsec Void String

csv :: String
csv = "col1,col2,col3\nr2 c1,r2 c2,r2 c3\n\"r3,c1\",\"r3,c2\",\"r3,c3\""

csvRes :: [[String]]
csvRes =
  [ [ "col1",  "col2",  "col3" ]
  , [ "r2 c1", "r2 c2", "r2 c3" ]
  , [ "r3,c1", "r3,c2", "r3,c3" ]
  ]

csvParser :: Parser [[String]]
csvParser = sepEndBy parseRow (char '\n')

parseRow :: Parser [String]
parseRow = sepEndBy parseFromValue (char ',')

parseFromValue :: Parser String
parseFromValue = parseFromValueWithQuotes <|> parseFromValueWithoutQuotes

parseFromValueWithoutQuotes :: Parser String
parseFromValueWithoutQuotes = many ((char '\\' >> anySingle) <|> noneOf ",\n")

parseFromValueWithQuotes :: Parser String
parseFromValueWithQuotes = char '"' *> many ((char '\\' >> anySingle) <|> noneOf "\"") <* char '"'

main :: IO ()
main = parseTest csvParser csv