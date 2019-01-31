
module Language.FIRRTL.Preprocessor where

import qualified Data.Text as T
import Language.FIRRTL.Tokens


groupStatements :: [Token] -> [Token]
groupStatements = go 0 0
  where
    go m n (x@(Token Tok_Colon _ _) : y@(Token Tok_Info _ _) : z@(Token Tok_LParen _ _) : xs) = x : y : z : go m n xs
    go m n (x@(Token Tok_Colon _ _) : y@(Token Tok_LParen _ _) : xs) = x : y : go m n xs
    go m _ (Token Tok_Colon s p : x@(Token Tok_Info _ _) : Token Tok_Newline _ _ : Token Tok_Indent i _ : xs) = Token Tok_Colon s p : x : Token Tok_LParen s p : go (succ m) (T.length i) xs
    go m _ (Token Tok_Colon s p : Token Tok_Newline _ _ : Token Tok_Indent i _ : xs) = Token Tok_Colon s p : Token Tok_LParen s p : go (succ m) (T.length i) xs
    go m n (Token Tok_Indent _ _ : Token Tok_Newline _ _ : xs) = go m n xs
    go m n (Token Tok_Indent  i p : xs) | T.length i == n = Token Tok_Comma i p : go m n xs
    go m n (Token Tok_Indent  i p : xs) | T.length i < n = Token Tok_RParen i p : go (pred m) n xs
    go m n (Token Tok_Newline _ _ : xs) = go m n xs
    go m n (Token Tok_Indent  _ _ : xs) = go m n xs
    go m n (x : xs) = x : go m n xs
    go m _ _ = replicate m (Token Tok_RParen mempty (Position "" 0 0))
