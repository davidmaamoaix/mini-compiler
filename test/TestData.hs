module TestData where

import System.IO
import qualified Data.Map as Map

import Test.HUnit

import Lexer

type StateRef = Map.Map String

tokenRef :: StateRef (Either () [TokenPos])
tokenRef = Right <$> Map.fromList entries
    where
        entries = [ ("basics.c0", basics)
                  , ("structs.c0", structs)
                  , ("number_error.c0", numberError)
                  , ("pointer.c0", pointer)
                  ]
        basics = []
        structs = []
        numberError = []
        pointer = []
