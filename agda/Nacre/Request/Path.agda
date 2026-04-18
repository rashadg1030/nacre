module Nacre.Request.Path where

open import Haskell.Prelude

postulate
  Text : Set

{-# FOREIGN AGDA2HS import Data.Text (Text) #-}

record ParamCodec (a : Set) : Set where
  no-eta-equality
  constructor MkParamCodec
  field
    encodeParam : a → Text
    decodeParam : Text → Maybe a

open ParamCodec public

{-# COMPILE AGDA2HS ParamCodec #-}

data Contract (x : Set) (a : Set) : Set₁ where
  Pure  : a → Contract x a
  Fmap  : {b : Set} → (b → a) → Contract x b → Contract x a
  Lmap  : {y : Set} → (y → x) → Contract y a → Contract x a
  Apply : {b : Set} → Contract x (b → a) → Contract x b → Contract x a
  Lit   : Text → Contract x a
  Param : {b : Set} → Text → ParamCodec b → Contract x a

{-# COMPILE AGDA2HS Contract #-}

lit : Text → Contract Text Text
lit = Lit

{-# COMPILE AGDA2HS lit #-}

param : {a : Set} → Text → ParamCodec a → Contract a a
param = Param

{-# COMPILE AGDA2HS param #-}

lmap : {x y a : Set} → (y → x) → Contract x a → Contract y a
lmap f c = Lmap f c

{-# COMPILE AGDA2HS lmap #-}

rmap : {x a b : Set} → (a → b) → Contract x a → Contract x b
rmap f c = Fmap f c

{-# COMPILE AGDA2HS rmap #-}

pure' : {x a : Set} → a → Contract x a
pure' = Pure

{-# COMPILE AGDA2HS pure' #-}

ap : {x a b : Set} → Contract x (a → b) → Contract x a → Contract x b
ap = Apply

{-# COMPILE AGDA2HS ap #-}
