{-# LANGUAGE TypeFamilies #-}

module Nacre.Function where

import Data.Kind (Type)
import Nacre.Request (Request)
import Nacre.Request qualified as Request
import Nacre.Responses qualified as Responses
import Network.Wai qualified as Wai

data Function (i :: Type) (o :: Type)

type (-->) = Function

data family Contract r

data family Server (ctx :: Type -> Type) r

data family Client r

data family Handler (ctx :: Type -> Type) r

data instance Contract (Request m p q b h --> o) where
    (:->) ::
        Request.Contract m p q b h ->
        Responses.Contract o ->
        Contract (Request m p q b h --> o)

(@->) ::
    (Request.Contract m p q b h -> Request.Contract m' p' q' b' h') ->
    (Responses.Contract o -> Responses.Contract o') ->
    (Contract (Request m p q b h --> o) -> Contract (Request m' p' q' b' h' --> o'))
(@->) = undefined

data instance Server ctx (Request m p q b h --> o) where
    (:=) ::
        Contract (Request m p q b h --> o) ->
        ((Request.Data m p q b h, Wai.Request) -> ctx o) ->
        Server ctx (Request m p q b h --> o)

(@=) ::
    (Contract (Request m p q b h --> o) -> Contract (Request m' p' q' b' h' --> o')) ->
    (((Request.Data m p q b h, Wai.Request) -> ctx o) -> ((Request.Data m' p' q' b' h', Wai.Request) -> ctx o')) ->
    (Server ctx (Request m p q b h --> o) -> Server ctx (Request m' p' q' b' h' --> o'))
(@=) = undefined

data instance Client (Request m p q b h --> o) where
    Client ::
        {fetch_ :: Request.Data m p q b h -> IO (Either ClientError o)} ->
        Client (Request m p q b h --> o)

data instance Handler ctx (Request m p q b h --> o) = Handler ((Request.Data m p q b h, Wai.Request) -> ctx o)

handler :: ((Request.Data m p q b h, Wai.Request) -> ctx o) -> Handler ctx (Request m p q b h --> o)
handler = Handler

data ClientError = ClientError
