{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nacre.Responses where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import GHC.Generics
import Nacre.Response qualified as Response
import Network.Wai qualified as Wai

data Contract o = Contract
    { encode :: o -> Wai.Response
    , decode :: Wai.Response -> Maybe o
    }

data GContract (rep :: Type -> Type) = GContract
    { gEncode :: forall x. rep x -> Wai.Response
    , gDecode :: forall x. Wai.Response -> Maybe (rep x)
    }

class GResponses (rep :: Type -> Type) where
    type Curried rep r :: Type
    gresponses :: (GContract rep -> r) -> Curried rep r

instance GResponses (K1 R (Response.Response s b h)) where
    type Curried (K1 R (Response.Response s b h)) r = Response.Contract s b h -> r
    gresponses k contract = k GContract
        { gEncode = \_k1 -> undefined
        , gDecode = \_resp -> undefined
        }

instance (GResponses a, GResponses b) => GResponses (a :+: b) where
    type Curried (a :+: b) r = Curried a (Curried b r)
    gresponses k = gresponses @a $ \gcLeft ->
        gresponses @b $ \gcRight ->
            k GContract
                { gEncode = \case
                    L1 x -> gEncode gcLeft x
                    R1 x -> gEncode gcRight x
                , gDecode = \resp ->
                    (L1 <$> gDecode gcLeft resp)
                        <|> (R1 <$> gDecode gcRight resp)
                }

instance GResponses rep => GResponses (M1 i meta rep) where
    type Curried (M1 i meta rep) r = Curried rep r
    gresponses k = gresponses @rep $ \gc ->
        k GContract
            { gEncode = \(M1 rep) -> gEncode gc rep
            , gDecode = \resp -> M1 <$> gDecode gc resp
            }

responses :: forall o -> (Generic o, GResponses (Rep o)) => Curried (Rep o) (Contract o)
responses o = gresponses @(Rep o) $ \gc -> Contract
    { encode = \(x :: o) -> gEncode gc (from x)
    , decode = \resp -> to <$> gDecode gc resp
    }

either ::
    forall s1 b1 h1 s2 b2 h2.
    Response.Contract s1 b1 h1 ->
    Response.Contract s2 b2 h2 ->
    Contract (Either (Response.Response s1 b1 h1) (Response.Response s2 b2 h2))
either = responses (type (Either (Response.Response s1 b1 h1) (Response.Response s2 b2 h2)))
