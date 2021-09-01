module TestUtils where

import System.IO

import Test.HUnit

type TestPhrase a = IO [a]