{-# LANGUAGE GADTs #-}

module Nacre.Request.Path where

import Data.Text (Text)

data ParamCodec a = MkParamCodec
  { encodeParam :: a -> Text
  , decodeParam :: Text -> Maybe a
  }

data Contract x a where
  Pure :: a -> Contract x a
  Fmap :: (a -> b) -> Contract x a -> Contract x b
  Lmap :: (y -> x) -> Contract x a -> Contract y a
  Apply :: Contract x (a -> b) -> Contract x a -> Contract x b
  Lit :: Text -> Contract Text Text
  Param :: Text -> ParamCodec a -> Contract a a

data RefinedContract a where
  Pure' :: a -> RefinedContract a
  Fmap' :: (a -> b) -> RefinedContract a -> RefinedContract b
  Apply' :: RefinedContract (a -> b) -> RefinedContract a -> RefinedContract b
  Refine :: (a -> Bool) -> Contract a a -> RefinedContract a

lit :: Text -> Contract Text Text
lit = Lit

param :: Text -> ParamCodec a -> Contract a a
param = Param

lmap :: (y -> x) -> Contract x a -> Contract y a
lmap = Lmap

rmap :: (a -> b) -> Contract x a -> Contract x b
rmap = Fmap

pure' :: a -> Contract x a
pure' = Pure

ap :: Contract x (a -> b) -> Contract x a -> Contract x b
ap = Apply

any :: Contract [Text] [Text]
any = undefined

none :: Contract () ()
none = undefined
