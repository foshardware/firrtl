{-# LANGUAGE OverloadedStrings #-}

module Language.FIRRTL.Syntax where

import Data.Text (Text)


type Identifier = Text


data Circuit = Circuit Identifier (Maybe Info) [Module]
  deriving (Eq, Show)

data Module
  = Module Identifier (Maybe Info) [Port] [Statement]
  | ExternalModule Identifier (Maybe Info) [Port] [Statement]
  deriving (Eq, Show)

data Port = Port Direction Identifier Type (Maybe Info)
  deriving (Eq, Show)

data Direction = Input | Output
  deriving (Eq, Show)

data Type
  = UIntType (Maybe Int)
  | SIntType (Maybe Int)
  | FixedType (Maybe Int) (Maybe Int)
  | ClockType
  | AnalogType (Maybe Int)
  | BundleType [Field]
  | VectorType Type Int
  deriving (Eq, Show)


data Field = Field (Maybe Flip) Key Type
  deriving (Eq, Show)

data Flip = Flip
  deriving (Eq, Show)

type Key = Identifier


data Statement
  = Wire Identifier Type (Maybe Info)
  | Register Identifier Type Exp (Maybe (Exp, Exp)) (Maybe Info)
  | Cmem Identifier Type (Maybe Info)
  | Smem Identifier Type (Maybe Info)
  | Memory Identifier (Maybe Info) Type Int Int Int RuW [Identifier] [Identifier] [Identifier]
  | Instance Identifier Identifier (Maybe Info)
  | Read Identifier Exp Identifier (Maybe Info)
  | Write Identifier Exp Identifier (Maybe Info)
  | Infer Identifier Exp Identifier (Maybe Info)
  | Node Identifier Exp (Maybe Info)
  | Connect Exp Exp (Maybe Info)
  | PartialConnect Exp Exp (Maybe Info)
  | Invalidate Exp (Maybe Info)
  | Attach [Exp] (Maybe Info)
  | Conditional Exp (Maybe Info) [Statement] (Maybe Info) [Statement]
  | Stop Exp Exp Int (Maybe Info)
  | Printf Exp Exp Text [Exp] (Maybe Info)
  | Skip (Maybe Info)
  | Defname Identifier
  | Parameter Identifier Exp
  deriving (Eq, Show)

data RuW = Old | New | Undefined
  deriving (Eq, Show)


newtype Info = Info Text
  deriving (Eq, Show)


data Number
  = UIntFromInt  (Maybe Int) Int
  | UIntFromBits (Maybe Int) Text
  | SIntFromInt  (Maybe Int) Int
  | SIntFromBits (Maybe Int) Text
  | Integer Int
  deriving (Eq, Show)

data Exp
  = Number Number
  | Reference Identifier
  | Subfield Exp Identifier
  | Subindex Exp Exp
  | Subaccess Exp Exp
  | ValidIf Exp Exp
  | PrimOp PrimOp [Exp]
  | String Text
  deriving (Eq, Show)


data PrimOp
  = Mux
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Leq
  | Gt
  | Geq
  | Eq
  | Neq
  | Pad
  | AsUInt
  | AsSInt
  | AsClock
  | Shl
  | Shr
  | Dshl
  | Dshr
  | Cvt
  | Neg
  | Not
  | And
  | Or
  | Xor
  | Andr
  | Orr
  | Xorr
  | Cat
  | Bits
  | Head
  | Tail
  deriving (Eq, Show)
