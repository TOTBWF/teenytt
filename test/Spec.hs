module Main where

import Test.Hspec

import Spec.TeenyTT.Base.SymbolTable qualified as SymbolTable

main :: IO ()
main = hspec $ do
    SymbolTable.specs
    SymbolTable.properties
