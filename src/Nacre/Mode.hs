{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nacre.Mode where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NEL
import Data.Profunctor
import Data.Text qualified as Text
import GHC.Generics (C1, D1, Generic (..), Rec0, Rep, S1, U1, V1, (:*:), (:+:))
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Web.HttpApiData qualified as HTTP

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

type IsoContract t a = Contract t a a

instance Functor (Contract t i) where
    fmap :: (a -> b) -> Contract t i a -> Contract t i b
    fmap = FMap

instance Applicative (Contract t i) where
    pure = Pure
    (<*>) = Apply

instance Profunctor (Contract t) where
    rmap = FMap
    lmap = LMap

cost ::
    forall t i o.
    Contract t i o ->
    Int
cost = \case
    FMap _ c -> cost c
    LMap _ c -> cost c
    Pure _ -> 0
    Apply c c' -> cost c + cost c'
    Embed _ -> 1

type Parser t a = StateOf t -> (Either (ParseErrorOf t) a, StateOf t)

parse ::
    forall t i o.
    (forall i' o'. t i' o' -> Parser t o') ->
    Contract t i o ->
    Parser t o
parse = undefined

isoParse ::
    forall t a.
    (forall b. t b b -> Parser t b) ->
    IsoContract t a ->
    Parser t a
isoParse = undefined

type Printer t a = a -> StateOf t

print ::
    forall t i o.
    (Monoid (StateOf t)) =>
    (forall i' o'. t i' o' -> Printer t i') ->
    Contract t i o ->
    Printer t i
print = undefined

isoPrint ::
    forall t a.
    (forall b. t b b -> Printer t b) ->
    IsoContract t a ->
    Printer t a
isoPrint = undefined

newtype Value (t :: Type -> Type -> Type) a = Value {value :: a}

-- foldContract ::
--     (Profunctor f, forall x. Applicative (f x)) =>
--     (forall i o. t i o -> f i o) ->
--     Contract t i o ->
--     f i o
-- foldContract alg = \case
--     FMap f c -> fmap f (foldContract alg c)
--     LMap f c -> lmap f (foldContract alg c)
--     Pure x -> pure x
--     Apply f x -> foldContract alg f <*> foldContract alg x
--     Embed t -> alg t

data
    Signature
        (m :: Type)
        (p :: Type)
        (q :: Type)
        (h :: Type)
        (b :: Type)
        (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)

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

-- EXAMPLE AREA
-- Response types
{-
data GetUserResponses f
    = GetUserOK       (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | GetUserNotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

data CreateUserResponses f
    = CreateUserOK    (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | CreateUserError (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

-- Signature type synonyms
type GetUserEndpoint = Signature
    HTTP.StdMethod
    UserId
    ()
    HTTP.RequestHeaders
    (IO LBS.ByteString)
    GetUserResponses

type CreateUserEndpoint = Signature
    HTTP.StdMethod
    ()
    ()
    HTTP.RequestHeaders
    (IO CreateUserBody)
    CreateUserResponses

-- API record
data UsersApi mode = UsersApi
    { getUser    :: mode GetUserEndpoint
    , createUser :: mode CreateUserEndpoint
    }

-- Contracts
usersContracts :: UsersApi Endpoint
usersContracts = UsersApi
    { getUser    = getUserRequest    :-> getUserResponses
    , createUser = createUserRequest :-> createUserResponses
    }

-- Handlers
usersServer :: UsersApi (Server IO)
usersServer = UsersApi
    { getUser = fn \(req, _) -> do
        user <- DB.fetchUser req.path.value
        pure (GetUserOK user)
    , createUser = fn \(req, _) -> do
        body   <- req.reqBody.value
        result <- DB.insertUser body
        pure (CreateUserOK result)
    }

data MyResponses f
    = OK       (Response f S200 HTTP.ResponseHeaders LBS.ByteString)
    | NotFound (Response f S404 HTTP.ResponseHeaders LBS.ByteString)
    | Error    (Response f S500 HTTP.ResponseHeaders LBS.ByteString)
    deriving (Generic, GenericResponses)

usersProductsPath = do
    Path.lit "users"
    userId    <- fst .= Path.param @UserId
    productId <- snd .= Path.param @ProductId
    pure (userId, productId)

myRequest
    = request
    . path' do -- /users/:userID/:productID/blah
        (userId, productId) <- usersProductsPath
        Path.lit "blah"
        pure (userId, productId)
    . query' do
        age <- Query.param @Int "age"
        Query.flag "male"
        pure age

myResponses = responses @MyResponses
    okResponse
    notFoundResponse
    errorResponse
    where
        okResponse
            = response
            . status' 200

myRoute = myRequest :-> myResponses

-- servant-named-routes
data MyApplication f = MyApplication
    { getUsers :: f ()
    , postUsers :: f ()
    , deleteUsers :: f ()
    }

-}
