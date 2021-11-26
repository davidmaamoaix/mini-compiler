module Lexer where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Char (toUpper)

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (token, tokens, try)

import Control.Applicative hiding (many, (<|>))
