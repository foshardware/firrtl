{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (evaluate)

import Data.FileEmbed
import Data.Text.Encoding

import Test.Tasty
import Test.Tasty.HUnit

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser


main :: IO ()
main = defaultMain $ testGroup "FIR"
  [ rocket_chip
  ]

rocket_chip :: TestTree
rocket_chip = testGroup "Rocket chip"
  [ testCase "test_harness.fir" $ parse $ circuit $ lexer [] $ decodeUtf8 $(embedFile "sample/test_harness.fir")
  ]

parse :: Show a => a -> IO ()
parse s = () <$ evaluate s
