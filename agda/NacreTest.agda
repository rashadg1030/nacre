module NacreTest where

open import Haskell.Prelude

someFunc : IO ⊤
someFunc = putStrLn "someFunc"

{-# COMPILE AGDA2HS someFunc #-}
