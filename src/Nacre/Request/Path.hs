module Nacre.Request.Path where

import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Nacre.Contract (Contract)

type IsoHttpApiData a = (HTTP.FromHttpApiData a, HTTP.ToHttpApiData a)

data Path i o where
    Lit :: Text -> Path () ()
    Param :: (Typeable a, IsoHttpApiData a) => Path a a
    Blob :: (Typeable a, IsoHttpApiData a) => Path (NonEmpty a) (NonEmpty a)

data ParseError = ParseError

parse :: Contract Path i o -> [Text] -> (Either ParseError o, [Text])
parse contract path = case contract of
    _ -> undefined

print :: Contract Path i o -> i -> [Text.Text]
print = undefined
