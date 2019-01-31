{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import Data.Text.Encoding

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser
import Language.FIRRTL.Preprocessor


main :: IO ()
main = putStrLn $ show $ circuit $ lexer [] $ decodeUtf8 $(embedFile "sample/test_harness.fir")

