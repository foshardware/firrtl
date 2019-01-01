{-# LANGUAGE OverloadedStrings #-}

module Language.FIRRTL.AST where


type Identifier = String


data Circuit = Circuit Identifier (Maybe Info) [Module]
  deriving (Eq, Show)

data Module
  = Module Identifier (Maybe Info) [Port] Statement
  | ExternalModule Identifier (Maybe Info) [Port]
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

data Field = Field (Maybe Flip) Identifier Type
  deriving (Eq, Show)

data Statement
  = Wire Identifier Type (Maybe Info)
  | Register Identifier Type Exp (Maybe (Exp, Exp)) (Maybe Info)
  | Memory Identifier (Maybe Info) Type Int Int Int RuW [Identifier] [Identifier] [Identifier]
  | Instance Identifier Identifier (Maybe Info)
  | Node Identifier Exp (Maybe Info)
  | Connect Exp Exp (Maybe Info)
  | PartialConnect Exp Exp (Maybe Info)
  | Invalidate Exp (Maybe Info)
  | Attach [Exp] (Maybe Info)
  | Conditional Exp (Maybe Info) Statement (Maybe Statement)
  | Stop Exp Exp Int (Maybe Info)
  | Printf Exp Exp String [Exp] (Maybe Info)
  | Empty (Maybe Info)
  | Group [Statement]
  deriving (Eq, Show)

data RuW = Old | New | Undefined
  deriving (Eq, Show)

data Flip = Flip
  deriving (Eq, Show)

newtype Info = Info String
  deriving (Eq, Show)

data Exp
  = UIntFromInt  (Maybe Int) Int
  | UIntFromBits (Maybe Int) String
  | SIntFromInt  (Maybe Int) Int
  | SIntFromBits (Maybe Int) String
  | Reference Identifier
  | Subfield Exp Identifier
  | Subindex Exp Int
  | Subaccess Exp Exp
  | Multiplexor Exp Exp Exp
  | ValidIf Exp Exp
  | PrimOp [Exp] [Int]
  deriving (Eq, Show)

