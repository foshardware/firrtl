{

module Language.FIRRTL.Parser where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

import Language.FIRRTL.Syntax
import Language.FIRRTL.Tokens

}

%name circuit
%tokentype { Token }
%error { parseError }


%token

indent         { Token Tok_Indent   _ _ }
dedent         { Token Tok_Dedent   _ _ }

"circuit"        { Token Tok_Circuit   _ _ }
"module"         { Token Tok_Module    _ _ }
"extmodule"      { Token Tok_Extmodule _ _ }
"flip"           { Token Tok_Flip      _ _ }
"input"          { Token Tok_Input     _ _ }
"output"         { Token Tok_Output    _ _ }

"when"           { Token Tok_When      _ _ }
"else"           { Token Tok_Else      _ _ }

"mem"            { Token Tok_Mem       _ _ }
"cmem"           { Token Tok_Cmem      _ _ }
"smem"           { Token Tok_Smem      _ _ }

"wire"           { Token Tok_Wire      _ _ }
"reg"            { Token Tok_Reg       _ _ }
"with"           { Token Tok_With      _ _ }
"node"           { Token Tok_Node      _ _ }
"stop"           { Token Tok_Stop      _ _ }
"skip"           { Token Tok_Skip      _ _ }

"reset"          { Token Tok_Reset     _ _ }
"printf"         { Token Tok_Printf    _ _ }

"infer"          { Token Tok_Infer     _ _ }
"mport"          { Token Tok_Mport     _ _ }

"mux"              { Token Tok_Mux     _ _ }
"add"              { Token Tok_Add     _ _ }
"sub"              { Token Tok_Sub     _ _ }
"mul"              { Token Tok_Mul     _ _ }
"div"              { Token Tok_Div     _ _ }
"mod"              { Token Tok_Mod     _ _ }
"lt"               { Token Tok_Lt      _ _ }
"leq"              { Token Tok_Leq     _ _ }
"gt"               { Token Tok_Gt      _ _ }
"geq"              { Token Tok_Geq     _ _ }
"eq"               { Token Tok_Eq      _ _ }
"neq"              { Token Tok_Neq     _ _ }
"pad"              { Token Tok_Pad     _ _ }
"shl"              { Token Tok_Shl     _ _ }
"shr"              { Token Tok_Shr     _ _ }
"dshl"             { Token Tok_Dshl    _ _ }
"dshr"             { Token Tok_Dshr    _ _ }
"cvt"              { Token Tok_Cvt     _ _ }
"neg"              { Token Tok_Neg     _ _ }
"not"              { Token Tok_Not     _ _ }
"and"              { Token Tok_And     _ _ }
"or"               { Token Tok_Or      _ _ }
"xor"              { Token Tok_Xor     _ _ }
"andr"             { Token Tok_Andr    _ _ }
"orr"              { Token Tok_Orr     _ _ }
"xorr"             { Token Tok_Xorr    _ _ }
"cat"              { Token Tok_Cat     _ _ }
"bits"             { Token Tok_Bits    _ _ }
"head"             { Token Tok_Head    _ _ }
"tail"             { Token Tok_Tail    _ _ }
"asUInt"           { Token Tok_AsUInt  _ _ }
"asSInt"           { Token Tok_AsSInt  _ _ }
"asClock"          { Token Tok_AsClock _ _ }

"is"             { Token Tok_Is        _ _ }
"invalid"        { Token Tok_Invalid   _ _ }
"inst"           { Token Tok_Inst      _ _ }
"of"             { Token Tok_Of        _ _ }


"defname"        { Token Tok_Defname   _ _ }
"parameter"      { Token Tok_Parameter _ _ }

"UInt"           { Token Tok_UInt      _ _ }
"SInt"           { Token Tok_SInt      _ _ }
"Fixed"          { Token Tok_Fixed     _ _ }
"Clock"          { Token Tok_Clock     _ _ }
"Analog"         { Token Tok_Analog    _ _ }

"=>"             { Token Tok_Op_Arrow  _ _ }

"<-"             { Token Tok_Op_Partial _ _ }
"<="             { Token Tok_Op_Connect _ _ }

"="              { Token Tok_Op_Eq  _ _  }

"."              { Token Tok_Dot    _ _ }
","              { Token Tok_Comma  _ _ }
":"              { Token Tok_Colon  _ _ }
"<"              { Token Tok_Op_Lt  _ _ }
">"              { Token Tok_Op_Gt  _ _ }

"<<"             { Token Tok_Op_Lt_Lt _ _ }
">>"             { Token Tok_Op_Gt_Gt _ _ }

"{"              { Token Tok_LBrace _ _ }
"}"              { Token Tok_RBrace _ _ }

"["              { Token Tok_LBrack _ _ }
"]"              { Token Tok_RBrack _ _ }

"("              { Token Tok_LParen _ _ }
")"              { Token Tok_RParen _ _ }

info             { Token Tok_Info   _ _ }
number           { Token Tok_Number _ _ }
simpleIdentifier { Token Tok_Ident  _ _ }
string           { Token Tok_String _ _ }

%%

Circuit :: { Circuit }
: "circuit" Identifier ":" opt(Info) indent many(Module) dedent { Circuit $2 $4 $6 }

Module :: { Module }
: "module" Identifier ":" opt(Info) indent many(Port) many(Stmt) dedent { Module $2 $4 $6 $7 }
| "extmodule" Identifier ":" opt(Info) indent many(Port) many(Stmt) dedent { ExternalModule $2 $4 $6 $7 }

Port :: { Port }
: Dir ComplexIdentifier ":" Type opt(Info) { Port $1 $2 $4 $5 }

Dir :: { Direction }
Dir
: "input"  { Input  }
| "output" { Output }

Type :: { Type }
: "UInt" opt(between("<", Int, ">")) { UIntType $2 }
| "SInt" opt(between("<", Int, ">")) { SIntType $2 }
| "Fixed" opt(between("<", Int, ">")) opt(between("<<", Int, ">>")) { FixedType $2 $3 }
| "Clock" { ClockType }
| "Analog" opt(between("<", Int, ">")) { AnalogType $2 }
| "{" csv(Field) "}" { BundleType $2 }
| Type "[" Int "]" { VectorType $1 $3 }

Field :: { Field }
: opt(Flip) ComplexIdentifier ":" Type { Field $1 $2 $4 }

Flip :: { Flip }
: "flip" { Flip }

Stmt :: { Statement }
: "wire" Identifier ":" Type opt(Info) { Wire $2 $4 $5 }
| "reg" Identifier ":" Type "," Exp opt(WithReset) opt(Info) { Register $2 $4 $6 $7 $8 }
| "cmem" Identifier ":" Type opt(Info) { Cmem $2 $4 $5 }
| "inst" Identifier "of" Identifier opt(Info) { Instance $2 $4 $5 }
| "infer" "mport" Identifier "=" Exp "," Identifier opt(Info) { Mport $3 $5 $7 $8 }
| Exp "is" "invalid" opt(Info) { Invalidate $1 $4 }
| Exp "<-" Exp opt(Info) { Connect $1 $3 $4 }
| Exp "<=" Exp opt(Info) { PartialConnect $1 $3 $4 }
| "when" Exp ":" opt(Info) indent many(Stmt) dedent "else" ":" opt(Info) indent many(Stmt) dedent { Conditional $2 $4 $6 $10 $12 }
| "when" Exp ":" opt(Info) indent many(Stmt) dedent { Conditional $2 $4 $6 Nothing [] }
| "node" Identifier "=" Exp opt(Info) { Node $2 $4 $5 }
| "defname" "=" Identifier { Defname $3 }
| "parameter" Identifier "=" Exp { Parameter $2 $4 }
| "stop" "(" Exp "," Exp "," Int ")" opt(Info) { Stop $3 $5 $7 $9 }
| "printf" "(" Exp "," Exp "," String "," csv(Exp) ")" opt(Info) { Printf $3 $5 $7 $9 $11 }
| "printf" "(" Exp "," Exp "," String ")" opt(Info) { Printf $3 $5 $7 [] $9 }
| "skip" opt(Info) { Skip $2 }

WithReset :: { (Exp, Exp) }
: "with" ":" "(" "reset" "=>" "(" Exp "," Exp ")" ")" { ($7, $9) }


Info :: { Info }
: info { Info(content $1) }


Exp :: { Exp }
: ComplexIdentifier { Reference $1 }
| Number { Number $1 }
| Exp "." ComplexIdentifier { Subfield $1 $3 }
| Exp "[" Exp "]" { Subindex $1 $3 }
| PrimOp "(" csv(Exp) ")" { PrimOp $1 $3 }
| String { String $1 }


Number :: { Number }
: "UInt" opt(between("<", Int, ">")) "(" Int    ")" { Right $ UIntFromInt  $2 $4 }
| "UInt" opt(between("<", Int, ">")) "(" String ")" { Right $ UIntFromBits $2 $4 }
| "SInt" opt(between("<", Int, ">")) "(" Int    ")" { Right $ SIntFromInt  $2 $4 }
| "SInt" opt(between("<", Int, ">")) "(" String ")" { Right $ SIntFromBits $2 $4 }
| Int { Left $1 }


PrimOp :: { PrimOp }
: "mux"              { Mux    }
| "add"              { Add    }
| "sub"              { Sub    }
| "mul"              { Mul    }
| "div"              { Div    }
| "mod"              { Mod    }
| "lt"               { Lt     }
| "leq"              { Leq    }
| "gt"               { Gt     }
| "geq"              { Geq    }
| "eq"               { Eq     }
| "neq"              { Neq    }
| "pad"              { Pad    }
| "shl"              { Shl    }
| "shr"              { Shr    }
| "dshl"             { Dshl   }
| "dshr"             { Dshr   }
| "cvt"              { Cvt    }
| "neg"              { Neg    }
| "not"              { Not    }
| "and"              { And    }
| "or"               { Or     }
| "xor"              { Xor    }
| "andr"             { Andr   }
| "orr"              { Orr    }
| "xorr"             { Xorr   }
| "cat"              { Cat    }
| "bits"             { Bits   }
| "head"             { Head   }
| "tail"             { Tail   }
| "asUInt"           { AsUInt   }
| "asSInt"           { AsSInt   }
| "asClock"          { AsClock  }



Int :: { Int }
: number { base10 $1 }

String :: { Text }
: string { strip $1 }


ComplexIdentifier :: { Identifier }
: Identifier { $1 }
| keywordIdentifier { $1 }

Identifier :: { Identifier }
: simpleIdentifier { content $1 }

keywordIdentifier :: { Identifier }
: "bits"   { pack "bits"   }
| "reset"  { pack "reset"  }




csv(p)
: sepBy1(p, ",") { $1 }
| { [] }

sepBy1(p, s)
: p s sepBy1(p, s){ $1 : $3 }
| p { [$1] }

many(p)
: p many(p) { $1 : $2 }
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

base10 :: Token -> Int
base10 = either (error . show) id . parse decimal "base10" . unpack . content

strip :: Token -> Text
strip = T.tail . T.init . content

}

