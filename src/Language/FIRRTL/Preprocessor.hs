
module Language.FIRRTL.Preprocessor where

import qualified Data.Text as T
import Language.FIRRTL.Tokens


groupStatements :: [Token] -> [Token]
groupStatements = go 0 0
  where
    go m n (Token Tok_Newline _ _ : Token Tok_Newline _ _ : Token Tok_Indent i p : xs)
      | T.length i < m
      = Token Tok_RParen i p : go m (pred n) xs
    go m n (Token Tok_Newline _ _ : Token Tok_Newline _ _ : Token Tok_Indent i p : xs)
      = go m n xs
    go m n (Token Tok_Colon _ _ : Token Tok_Newline _ _ : Token Tok_Indent i p : xs)
      = Token Tok_Colon i p : Token Tok_LParen i p : go (T.length i) (succ n) xs
    go m n (Token Tok_Newline _ _ : Token Tok_Indent i p : xs)
      | T.length i < m
      = Token Tok_RParen i p : go m n xs
    go m n (Token Tok_Newline _ _ : Token Tok_Indent i p : xs)
      = Token Tok_Comma i p : go m n xs
    go m n (Token Tok_Newline _ _ : xs)
      = go m n xs
    go m n (x : xs)
      = x : go m n xs
    go m _ _
      = replicate m (Token Tok_RParen mempty (Position "" 0 0))
