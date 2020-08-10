module RIR.Parser.STerm
  ( Term(..)
  , readTerm
  )
where

import           Control.Monad
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import qualified Text.ParserCombinators.Parsec.Token
                                               as T
import           Text.ParserCombinators.Parsec.Language
                                                ( emptyDef )

{- | AST for S-expression terms.
-}
data Term
    = TermAtom String
    | TermList [Term]
    deriving (Eq, Show)

readTerm = parse (spaces >> termParser) "R-lang"

termParser :: Parser Term
termParser = atomParser <|> listParser

atomParser :: Parser Term
atomParser = do
  atom <- many1 (letter <|> digit <|> symbol)
  return $ TermAtom atom

listParser :: Parser Term
listParser = parensList <|> bracketsList
 where
  parensList = do
    char '('
    spaces
    trms <- termParser `sepBy` spaces
    spaces
    char ')'
    return $ TermList trms
  bracketsList = do
    char '['
    spaces
    trms <- termParser `sepBy` spaces
    spaces
    char ']'
    return $ TermList trms

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space
