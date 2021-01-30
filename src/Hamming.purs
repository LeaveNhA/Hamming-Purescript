module Hamming
  ( distance
  ) where

import Prelude

import Data.Array (filter, length, zip)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints as SCP
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

isEq :: Tuple Char Char -> Boolean
isEq (Tuple a b) | a == b = true
isEq _ = false

distance :: String -> String -> Maybe Int
distance dnaA dnaB | SCP.length dnaA == SCP.length dnaB =
  zip (toCharArray dnaA) (toCharArray dnaB)
  # (map isEq)
  # (filter ((==) false))
  # length
  # Just
distance _ _ = Nothing
