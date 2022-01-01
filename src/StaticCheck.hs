{-# LANGUAGE GADTs #-}

module StaticCheck where

import Parser

staticCheck :: Node Prog -> Maybe (Int, String)
staticCheck (NProg xs) = Nothing
