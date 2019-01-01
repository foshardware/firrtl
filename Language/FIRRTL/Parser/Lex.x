{
{-# OPTIONS_GHC -w #-}
module Language.FIRRTL.Parser.Lex
  ( lexer
  ) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Language.FIRRTL.Parser.Tokens

}

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]
@binaryDigit  = [0-1]
@octalDigit   = [0-7]
@hexDigit     = [0-9a-fA-F]

$sign = [\+\-]

@decimalBase = "'" [dD]
@binaryBase  = "'" [bB]
@octalBase   = "'" [oO]
@hexBase     = "'" [s]? [hH]

@binaryValue         = @binaryDigit ("_" | @binaryDigit)*
@octalValue  = @octalDigit  ("_" | @octalDigit)*
@hexValue    = @hexDigit    ("_" | @hexDigit)*

@unsignedNumber = $decimalDigit ("_" | $decimalDigit)*

@size = $sign? @unsignedNumber

@decimalNumber
  = @unsignedNumber
  | @size? @decimalBase @unsignedNumber

@binaryNumber = @size? @binaryBase @binaryValue
@octalNumber  = @size? @octalBase  @octalValue
@hexNumber    = @size? @hexBase @hexValue
  
-- $exp  = [eE]

-- @realNumber = unsignedNumber "." unsignedNumber | unsignedNumber ( "." unsignedNumber)? exp sign? unsignedNumber
@number = @decimalNumber | @octalNumber | @binaryNumber | @hexNumber

-- Strings

@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  @comment           ;

  $white             ;

  @decimalNumber     { tok Tok_Number }

  \@\[ @string \]    { tok Tok_Info   }
  @simpleIdentifier  { tok Tok_Ident  }

  "UInt"             { tok Tok_UInt   }
  "SInt"             { tok Tok_SInt   }
  "Fixed"            { tok Tok_Fixed  }
  "Clock"            { tok Tok_Clock  }
  "Analog"           { tok Tok_Analog }

  "circuit"          { tok Tok_Circuit   }
  "module"           { tok Tok_Module    }
  "extmodule"        { tok Tok_Extmodule }
  "input"            { tok Tok_Input     }
  "output"           { tok Tok_Output    }

  "flip"             { tok Tok_Flip  }
  "with"             { tok Tok_With  }
  "reset"            { tok Tok_Reset }

  "wire"             { tok Tok_Wire  }
  "reg"              { tok Tok_Reg   }

  "data-type"          { tok Tok_DataType       }
  "depth"              { tok Tok_Depth          }
  "read-latency"       { tok Tok_ReadLatency    }
  "write-latency"      { tok Tok_WriteLatency   }
  "read-under-write"   { tok Tok_ReadUnderWrite }
  "reader"             { tok Tok_Reader         }
  "writer"             { tok Tok_Writer         }
  "readwriter"         { tok Tok_Readwriter     }

  "inst"               { tok Tok_Inst }
  "node"               { tok Tok_Node }

  "invalid"          { tok Tok_Invalid }
  "attach"           { tok Tok_Attach  }
 
  "stop"             { tok Tok_Stop   }
  "printf"           { tok Tok_Printf }
  "skip"             { tok Tok_Skip   }

  "old"              { tok Tok_Old       }
  "new"              { tok Tok_New       }
  "undefined"        { tok Tok_Undefined }
 
  "when"             { tok Tok_When }
  "else"             { tok Tok_Else }

  "of"               { tok Tok_Of }
  "is"               { tok Tok_Is }

  "="                { tok Tok_Op_Eq   }
  ":"                { tok Tok_Colon   }
  ","                { tok Tok_Comma   }

  "}"                { tok Tok_RBrace  }
  "{"                { tok Tok_LBrace  }

  "]"                { tok Tok_RBrack  }
  "["                { tok Tok_LBrack  }

  ")"                { tok Tok_RParen  }
  "("                { tok Tok_LParen  }

  "<"                { tok Tok_Op_Lt  }
  ">"                { tok Tok_Op_Gt  }

  "=>"               { tok Tok_Op_Arrow }

  "<="               { tok Tok_Op_Connect }
  "<-"               { tok Tok_Op_Partial }

  "validif"          { tok Tok_ValidIf }

  "mux"              { tok Tok_Mux }

  "add"              { tok Tok_Add    }
  "sub"              { tok Tok_Sub    }
  "mul"              { tok Tok_Mul    }
  "div"              { tok Tok_Div    }
  "mod"              { tok Tok_Mod    }
  "lt"               { tok Tok_Lt     }
  "leq"              { tok Tok_Leq    }
  "gt"               { tok Tok_Gt     }
  "geq"              { tok Tok_Geq    }
  "eq"               { tok Tok_Eq     }
  "neq"              { tok Tok_Neq    }
  "pad"              { tok Tok_Pad    }
  "asUInt"           { tok Tok_AsUInt     }
  "asSInt"           { tok Tok_AsSInt     }
  "asClock"          { tok Tok_AsClock    }
  "shl"              { tok Tok_Shiftleft     }
  "shr"              { tok Tok_Shiftright    }
  "dshl"             { tok Tok_DynShiftleft  }
  "dshr"             { tok Tok_DynShiftright }
  "cvt"              { tok Tok_Convert }
  "neg"              { tok Tok_Neg  }
  "not"              { tok Tok_Not  }
  "and"              { tok Tok_And  }
  "or"               { tok Tok_Or   }
  "xor"              { tok Tok_Xor  }
  "andr"             { tok Tok_AndR }
  "orr"              { tok Tok_OrR  }
  "xorr"             { tok Tok_XorR }
  "cat"              { tok Tok_Cat  }
  "bits"             { tok Tok_Bits }
  "head"             { tok Tok_Head }
  "tail"             { tok Tok_Tail }

  .                  { tok Tok_Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c

lexer :: String -> T.Text -> [Token]
lexer file text = go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> act pos (T.unpack $ T.take len cs) : go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
}


