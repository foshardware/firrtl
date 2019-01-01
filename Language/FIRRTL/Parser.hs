
module Language.FIRRTL.Parser where

import Data.Text

import Language.FIRRTL.AST
import Language.FIRRTL.Parser.Lex
import Language.FIRRTL.Parser.Parse
import Language.FIRRTL.Parser.Tokens

parseFile :: [(Text, Text)] -> FilePath -> Text -> Circuit
parseFile _ file content = circuit tokens
  where
  tokens = fmap relocate $ lexer file content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

