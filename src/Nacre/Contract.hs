{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nacre.Contract where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NEL
import Data.Profunctor
import Data.Text qualified as Text
import GHC.Generics (C1, D1, Generic (..), Rec0, Rep, S1, U1, V1, (:*:), (:+:))
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Web.HttpApiData qualified as HTTP

data GET = GET

data POST = POST

data DELETE = DELETE

data Request (f :: (Type -> Type -> Type) -> Type -> Type) m p q h b = Request
    { method :: f Method m
    , path :: f Path p
    , query :: f Query q
    , reqHeaders :: f ReqHeaders h
    , reqBody :: f ReqBody b
    }

data S200 = S200

data S404 = S404

data S500 = S500

data Response (f :: (Type -> Type -> Type) -> Type -> Type) s h b = Response
    { status :: f Status s
    , resHeaders :: f ResHeaders h
    , resBody :: f ResBody b
    }

request :: Request IsoContract HTTP.StdMethod [Text.Text] HTTP.Query HTTP.RequestHeaders LBS.ByteString
request = undefined

response :: Response IsoContract HTTP.Status HTTP.ResponseHeaders LBS.ByteString
response = undefined

type Contract :: (Type -> Type -> Type) -> Type -> Type -> Type
data Contract t i o where
    FMap ::
        (o -> o') ->
        Contract t i o ->
        Contract t i o'
    LMap ::
        (i -> i') ->
        Contract t i' o ->
        Contract t i o
    Pure ::
        o ->
        Contract t i o
    Apply ::
        Contract t i (o -> o') ->
        Contract t i o ->
        Contract t i o'
    Embed ::
        t i o ->
        Contract t i o

instance Functor (Contract t i) where
    fmap :: (a -> b) -> Contract t i a -> Contract t i b
    fmap = FMap

instance Applicative (Contract t i) where
    pure = Pure
    (<*>) = Apply

instance Profunctor (Contract t) where
    rmap = FMap
    lmap = LMap

newtype Value (t :: Type -> Type -> Type) a = Value {value :: a}

newtype IsoContract (t :: Type -> Type -> Type) a = IsoContract {isoContract :: Contract t a a}

type IsoHttpApiData a = (HTTP.FromHttpApiData a, HTTP.ToHttpApiData a)

data Method i o

data Path i o where
    Lit :: Text.Text -> Path () ()
    Param :: IsoHttpApiData a => Path a a

-- Blob  :: IsoHttpApiData a => Path (NEL.NonEmpty a) (NEL.NonEmpty a)

data PathError = CouldntMatchLit Text.Text

parsePath :: Contract Path i o -> [Text.Text] -> (Either PathError o, [Text.Text])
parsePath contract path = case contract of
    _ -> undefined

data Query i o

data ReqHeaders i o

data ReqBody i o

data Status i o

data ResHeaders i o

data ResBody i o

data Responses i o

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

type family GCode (f :: Type -> Type) :: [[Type]] where
    GCode (D1 _ f) = GCode f
    GCode (f :+: g) = GCode f ++ GCode g
    GCode (C1 _ f) = '[GFields f]
    GCode V1 = '[]

type family GFields (f :: Type -> Type) :: [Type] where
    GFields (f :*: g) = GFields f ++ GFields g
    GFields (S1 _ f) = '[GField f]
    GFields U1 = '[]

type family GField (f :: Type -> Type) :: Type where
    GField (Rec0 a) = a

type family ResponseContract (xs :: [Type]) :: Type where
    ResponseContract '[Response Value s h b] = Response IsoContract s h b

type family ResponseContracts (xss :: [[Type]]) :: [Type] where
    ResponseContracts '[] = '[]
    ResponseContracts (xs : xss) = ResponseContract xs : ResponseContracts xss

type family ResponseChains (args :: [Type]) (result :: Type) :: Type where
    ResponseChains '[] result = result
    ResponseChains (a : as) result = a -> ResponseChains as result

class
    (Generic (r Value)) =>
    GenericResponses (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)
    where
    responses ::
        ResponseChains
            (ResponseContracts (GCode (Rep (r Value))))
            (IsoContract Responses (r Value))
    responses = undefined

data
    Signature
        (m :: Type)
        (p :: Type)
        (q :: Type)
        (h :: Type)
        (b :: Type)
        (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)

-- | In case the user just wants to return the Raw Response. A Raw Response provides no
-- type information about the response, besides the structure provided by Wai.Response.
-- Might not need it honestly because you just use a Response with the base types for everything
-- to represent an "any" response
data ResponseRaw (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)
    = ResponseRaw Wai.Response

{-
r needs to be higher kinded because it needs to be a sum type where every constructor
is a Response, and Response needs to take the Contract or Value tag:

type MyResponses :: ((Type -> Type -> Type) -> Type -> Type) -> Type
data MyResponses f
  = OK (Response f S200 ...)
  | NotFound (Response f S404 ...)
-}

data ClientError

data Endpoint sig where
    (:->) ::
        Request IsoContract m p q h b ->
        IsoContract Responses (r Value) ->
        Endpoint (Signature m p q h b r)

data Client sig where
    Callback ::
        (Request Value m p q h b -> IO (Either ClientError (r Value))) ->
        Client (Signature m p q h b r)

data Server n sig where
    Function ::
        ((Request Value m p q h b, Wai.Request) -> n (r Value)) ->
        Server n (Signature m p q h b r)

fn ::
    ((Request Value m p q h b, Wai.Request) -> n (r Value)) ->
    Server n (Signature m p q h b r)
fn = Function
