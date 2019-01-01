{
module Language.FIRRTL.Parser.Parse where

import Data.Bits
import Data.List

import Language.FIRRTL.AST
import Language.FIRRTL.Parser.Tokens
}

%name circuit
%tokentype { Token }
%error { parseError }

%expect 0

%token

"add"           { Token Tok_Add     _ _ }

%%

Circuit :: { Circuit  }
:  { Circuit mempty Nothing mempty }


{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString

}

