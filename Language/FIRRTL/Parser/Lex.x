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


