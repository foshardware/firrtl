
module Language.FIRRTL.Parser.Tokens where

import Data.Text
import Text.Printf

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token = Token TokenName String Position deriving (Show, Eq)

data TokenName
  = Tok_Add 
  | Tok_Unknown
  deriving (Show, Eq)

