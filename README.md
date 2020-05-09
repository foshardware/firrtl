# FIRRTL parser

Parses Flexible Intermediate Representation for RTL (FIRRTL) into an Abstract Syntax Tree (AST). Intended for use with LibreSilicon Compiler (lsc).

## Usage

```haskell

import Data.Text

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser
import Language.FIRRTL.Syntax


newtype FIR = FIR Circuit
  deriving (Eq, Show)

parseFIR :: Text -> Either ParseError FIR
parseFIR = fmap FIR . circuit . lexer []


```
