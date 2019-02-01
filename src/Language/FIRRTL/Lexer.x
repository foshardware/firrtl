{
{-# OPTIONS_GHC -w #-}
module Language.FIRRTL.Lexer
  ( lexer
  ) where

import Control.Monad.State
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Language.FIRRTL.Tokens

}

$any     = [.\n\r]
@newline = [\n\r] | \r\n

$space = [\ \t]

@comment = \;[^\r\n]*



-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]

$sign = [\+\-]

@decimalNumber = $decimalDigit+


-- Strings

@string_unquoted = [^\r\n]*
@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  @newline           ;

  @newline $space+   { indentation }

  ^@comment          ;
  $white             ;

  @decimalNumber     { tok Tok_Number }

  \@\[ @string_unquoted \]  { tok Tok_Info   }
  \@\[ @string \]           { tok Tok_Info   }

  @string            { tok Tok_String }

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
 
  "defname"          { tok Tok_Defname }
  "parameter"        { tok Tok_Parameter }

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
  "."                { tok Tok_Dot     }

  "}"                { tok Tok_RBrace  }
  "{"                { tok Tok_LBrace  }

  "]"                { tok Tok_RBrack  }
  "["                { tok Tok_LBrack  }

  ")"                { tok Tok_RParen  }
  "("                { tok Tok_LParen  }

  "<"                { tok Tok_Op_Lt  }
  ">"                { tok Tok_Op_Gt  }

  "<<"               { tok Tok_Op_Lt_Lt }
  ">>"               { tok Tok_Op_Gt_Gt }

  "=>"               { tok Tok_Op_Arrow }

  "<="               { tok Tok_Op_Connect }
  "<-"               { tok Tok_Op_Partial }

  "validif"          { tok Tok_ValidIf }
  "reset"            { tok Tok_Reset   }

  "mem"              { tok Tok_Mem     }
  "smem"             { tok Tok_Smem    }
  "cmem"             { tok Tok_Cmem    }

  "infer"            { tok Tok_Infer   }
  "mport"            { tok Tok_Mport   }

  "mux"              { tok Tok_Mux    }
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
  "shl"              { tok Tok_Shl    }
  "shr"              { tok Tok_Shr    }
  "dshl"             { tok Tok_Dshl   }
  "dshr"             { tok Tok_Dshr   }
  "cvt"              { tok Tok_Cvt    }
  "neg"              { tok Tok_Neg    }
  "not"              { tok Tok_Not    }
  "and"              { tok Tok_And    }
  "or"               { tok Tok_Or     }
  "xor"              { tok Tok_Xor    }
  "andr"             { tok Tok_Andr   }
  "orr"              { tok Tok_Orr    }
  "xorr"             { tok Tok_Xorr   }
  "cat"              { tok Tok_Cat    }
  "bits"             { tok Tok_Bits   }
  "head"             { tok Tok_Head   }
  "tail"             { tok Tok_Tail   }
  "asUInt"           { tok Tok_AsUInt   }
  "asSInt"           { tok Tok_AsSInt   }
  "asClock"          { tok Tok_AsClock  }


  @simpleIdentifier  { tok Tok_Ident  }

  .                  { tok Tok_Unknown }

{

wrap :: (T.Text -> TokenName) -> AlexPosn -> T.Text -> P Token
wrap f (AlexPn _ line col) s = pure $ Token (f s) s (Position "" line col)

tok = wrap . const
bstrTok f = wrap (f . T.encodeUtf8)
textTok = wrap
charTok f = wrap (f . T.head)

lexer :: String -> T.Text -> [Token]
lexer file text = filter emptyToken $ evalP $ go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> dedentRest pos
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> (:) <$> act pos (T.take len cs) <*> go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

    emptyToken Empty = False
    emptyToken _ = True

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

type P a = State (Int, Int) a

evalP :: P a -> a
evalP m = evalState m (0, 0)

indentation :: AlexPosn -> T.Text -> P Token
indentation (AlexPn _ l c) s = do
  (n, m) <- get
  case T.length s `compare` n of
    EQ -> pure Empty
    LT -> do
      put (T.length s, pred m)
      pure $ Token Tok_Dedent s (Position "" l c)
    GT -> do
      put (T.length s, succ m)
      pure $ Token Tok_Indent s (Position "" l c)


dedentRest :: AlexPosn -> P [Token]
dedentRest (AlexPn _ l c) = do
  (_, m) <- get
  put (0, 0)
  pure $ replicate m $ Token Tok_Dedent mempty (Position "" l c)
  

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


