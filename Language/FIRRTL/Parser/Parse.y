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

"add"            { Token Tok_Add       _ _ }
"circuit"        { Token Tok_Circuit   _ _ }
"module"         { Token Tok_Module    _ _ }
"extmodule"      { Token Tok_Extmodule _ _ }

"input"          { Token Tok_Input     _ _ }
"output"         { Token Tok_Output    _ _ }

"Clock"          { Token Tok_Clock     _ _ }

":"              { Token Tok_Colon  _ _ }

info             { Token Tok_Info  _ _ }
simpleIdentifier { Token Tok_Ident _ _ }

%%

Circuit :: { Circuit }
: "circuit" Identifier opt(Info) many(Module) { Circuit $2 $3 $4 }

Module :: { Module }
: "module" Identifier ":" opt(Info) many(Port) Stmt { Module $2 $4 $5 $6 }
| "extmodule" Identifier ":" opt(Info) many(Port) { ExternalModule $2 $4 $5 }

Port :: { Port }
: Dir Identifier ":" Type opt(Info) { Port $1 $2 $4 $5 }

Dir :: { Direction }
Dir
: "input"  { Input  }
| "output" { Output }

Type :: { Type }
: "Clock" { ClockType }

Stmt :: { Statement }
:  { Group [] }

Info :: { Info }
: info { Info(tokenString $1) }

Identifier :: { Identifier }
: simpleIdentifier { tokenString $1 }


sepBy1(p, s)
: sepBy1(p, s) s p { $3 : $1 }
| p { [$1] }

many(p)
: many(p) p { $2 : $1 }
| { [] } 

opt(p)
: p { Just $1 }
|   { Nothing }


{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString

}

