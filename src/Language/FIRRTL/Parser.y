{

module Language.FIRRTL.Parser where

import Data.Text (Text, unpack)

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

import Language.FIRRTL.Syntax
import Language.FIRRTL.Tokens

}

%name circuit
%tokentype { Token }
%error { parseError }


%token

"add"            { Token Tok_Add       _ _ }
"circuit"        { Token Tok_Circuit   _ _ }
"module"         { Token Tok_Module    _ _ }
"extmodule"      { Token Tok_Extmodule _ _ }
"flip"           { Token Tok_Flip      _ _ }
"input"          { Token Tok_Input     _ _ }
"output"         { Token Tok_Output    _ _ }

"wire"           { Token Tok_Wire      _ _ }

"is"             { Token Tok_Is        _ _ }
"invalid"        { Token Tok_Invalid   _ _ }

"UInt"           { Token Tok_UInt      _ _ }
"SInt"           { Token Tok_SInt      _ _ }
"Fixed"          { Token Tok_Fixed     _ _ }
"Clock"          { Token Tok_Clock     _ _ }
"Analog"         { Token Tok_Analog    _ _ }

"<-"             { Token Tok_Op_Partial _ _ }
"<="             { Token Tok_Op_Connect _ _ }

"."              { Token Tok_Dot    _ _ }
","              { Token Tok_Comma  _ _ }
":"              { Token Tok_Colon  _ _ }
"<"              { Token Tok_Op_Lt  _ _ }
">"              { Token Tok_Op_Gt  _ _ }

"{"              { Token Tok_LBrace _ _ }
"}"              { Token Tok_RBrace _ _ }

"["              { Token Tok_LBrack _ _ }
"]"              { Token Tok_RBrack _ _ }

"("              { Token Tok_LParen _ _ }
")"              { Token Tok_RParen _ _ }

info             { Token Tok_Info   _ _ }
number           { Token Tok_Number _ _ }
simpleIdentifier { Token Tok_Ident  _ _ }

%%

Circuit :: { Circuit }
: "circuit" Identifier ":" opt(Info) "(" csv(Module) ")" { Circuit $2 $4 $6 }

Module :: { Module }
: "module" Identifier ":" opt(Info) "(" csv(Port) csv(Stmt) ")" { Module $2 $4 $6 $7 }
| "extmodule" Identifier ":" opt(Info) "(" csv(Port) ")" { ExternalModule $2 $4 $6 }

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
| "{" csv(Field) "}" { BundleType $2 }
| Type "[" Int "]" { VectorType $1 $3 }

Field :: { Field }
: opt(Flip) Identifier ":" Type { Field $1 $2 $4 }

Flip :: { Flip }
: "flip" { Flip }

Stmt :: { Statement }
: "wire" Identifier ":" Type opt(Info) { Wire $2 $4 $5 }
| Exp "is" "invalid" opt(Info) { Invalidate $1 $4 }
| Exp "<-" Exp opt(Info) { Connect $1 $3 $4 }
| Exp "<=" Exp opt(Info) { PartialConnect $1 $3 $4 }

Info :: { Info }
: info { Info(content $1) }

Exp :: { Exp }
: Identifier { Reference $1 }
| Exp "." Identifier { Subfield $1 $3 }
| Exp "[" Int "]" { Subindex $1 $3 }


Int :: { Int }
: number { integer $1 }

Identifier :: { Identifier }
: simpleIdentifier { content $1 }


csv(p)
: sepBy1(p, ",") { $1 }
| { [] }

sepBy1(p, s)
: p s sepBy1(p, s){ $1 : $3 }
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
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ unpack s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

integer :: Token -> Int
integer = either (error . show) id . parse decimal "integer" . unpack . content

}

