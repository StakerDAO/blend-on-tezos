module Main
  ( main
  ) where

import Prelude (IO, (>>=))

import Test.Tasty (defaultMainWithIngredients)

import Cleveland.Ingredients (ourIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ourIngredients
