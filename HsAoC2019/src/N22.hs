module N22 () where

import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type SParser = Parsec Void String

data ShuffleType = NewStack | Inc Int | Cut Int deriving (Show)
numParser :: SParser Int
numParser = L.signed spaceConsumer (lexeme L.decimal)
 where
  lexeme = L.lexeme spaceConsumer
  spaceConsumer = L.space hspace1 empty empty

shuffleParser :: SParser ShuffleType
shuffleParser =
  let incParser = string "deal with increment " *> (Inc <$> L.decimal)
      stackParser = NewStack <$ string "deal into new stack"
      cutParser = string "cut " *> (Cut <$> numParser)
   in incParser <|> stackParser <|> cutParser

parseFile :: String -> [ShuffleType]
parseFile file = fromRight [] $ runParser (sepEndBy shuffleParser newline) "" file
