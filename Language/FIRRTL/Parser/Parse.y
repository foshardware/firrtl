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

%expect 1

%token

"add"            { Token Tok_Add       _ _ }
"circuit"        { Token Tok_Circuit   _ _ }
"module"         { Token Tok_Module    _ _ }
"extmodule"      { Token Tok_Extmodule _ _ }
"flip"           { Token Tok_Flip      _ _ }
"input"          { Token Tok_Input     _ _ }
"output"         { Token Tok_Output    _ _ }

"UInt"           { Token Tok_UInt      _ _ }
"SInt"           { Token Tok_SInt      _ _ }
"Fixed"          { Token Tok_Fixed     _ _ }
"Clock"          { Token Tok_Clock     _ _ }
"Analog"         { Token Tok_Analog    _ _ }

":"              { Token Tok_Colon  _ _ }
"<"              { Token Tok_Op_Lt  _ _ }
">"              { Token Tok_Op_Gt  _ _ }

"{"              { Token Tok_LBrace _ _ }
"}"              { Token Tok_RBrace _ _ }

"["              { Token Tok_LBrack _ _ }
"]"              { Token Tok_RBrack _ _ }

info             { Token Tok_Info   _ _ }
number           { Token Tok_Number _ _ }
simpleIdentifier { Token Tok_Ident  _ _ }

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
: "UInt" opt(between("<", Int, ">")) { UIntType $2 }
| "SInt" opt(between("<", Int, ">")) { SIntType $2 }
| "Fixed" opt(between("<", Int, ">"))
    opt(between("<", between("<", Int, ">"), ">")) { FixedType $2 $3 }
| "Clock" { ClockType }
| "Analog" opt(between("<", Int, ">")) { AnalogType $2 }
| "{" many(Field) "}" { BundleType $2 }
| Type "[" Int "]" { VectorType $1 $3 }

Field :: { Field }
: opt(Flip) Identifier ":" Type { Field $1 $2 $4 }

Flip :: { Flip }
: "flip" { Flip }

Stmt :: { Statement }
:  { Group [] }

Info :: { Info }
: info { Info(tokenString $1) }

Int :: { Int }
: number { read(tokenString $1) }

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

between(a, p, b)
: a p b { $2 }

{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString

}

