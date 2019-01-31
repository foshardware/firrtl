
module Language.FIRRTL.Tokens where

import Data.Text (Text)
import Text.Printf

content :: Token -> Text
content (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token = Token TokenName Text Position deriving (Show, Eq)

data TokenName
  = Tok_Undefined
  | Tok_Newline
  | Tok_Indent
  | Tok_Number
  | Tok_Info
  | Tok_Ident
  | Tok_Fixed
  | Tok_UInt
  | Tok_SInt
  | Tok_Clock
  | Tok_Analog
  | Tok_Circuit
  | Tok_Module
  | Tok_Extmodule
  | Tok_Input
  | Tok_Output
  | Tok_Flip
  | Tok_With
  | Tok_Reset
  | Tok_Wire
  | Tok_Reg
  | Tok_DataType
  | Tok_Depth
  | Tok_ReadLatency
  | Tok_WriteLatency
  | Tok_ReadUnderWrite
  | Tok_Reader
  | Tok_Writer
  | Tok_Readwriter
  | Tok_Inst
  | Tok_Node
  | Tok_Invalid
  | Tok_Attach
  | Tok_Stop
  | Tok_Printf
  | Tok_Skip
  | Tok_Old
  | Tok_New
  | Tok_When
  | Tok_Else
  | Tok_Of
  | Tok_Is
  | Tok_Op_Eq
  | Tok_Colon
  | Tok_Comma
  | Tok_RBrace
  | Tok_LBrace
  | Tok_RBrack
  | Tok_LBrack
  | Tok_RParen
  | Tok_LParen
  | Tok_Op_Lt
  | Tok_Op_Gt
  | Tok_Op_Arrow
  | Tok_Op_Connect
  | Tok_Op_Partial
  | Tok_ValidIf
  | Tok_Mux
  | Tok_Add
  | Tok_Sub
  | Tok_Mul
  | Tok_Div
  | Tok_Mod 
  | Tok_Lt 
  | Tok_Leq 
  | Tok_Gt 
  | Tok_Geq 
  | Tok_Eq 
  | Tok_Neq 
  | Tok_Pad 
  | Tok_AsUInt 
  | Tok_AsSInt 
  | Tok_AsClock 
  | Tok_Shiftleft 
  | Tok_Shiftright 
  | Tok_DynShiftleft 
  | Tok_DynShiftright 
  | Tok_Convert 
  | Tok_Neg 
  | Tok_Not 
  | Tok_And 
  | Tok_Or 
  | Tok_Xor 
  | Tok_AndR 
  | Tok_OrR 
  | Tok_XorR 
  | Tok_Cat 
  | Tok_Bits 
  | Tok_Head 
  | Tok_Tail 
  | Tok_Unknown
  deriving (Show, Eq)

