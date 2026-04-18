{-# LANGUAGE TypeFamilies #-}

module Nacre.Function where

import Data.Kind (Type)
import Nacre.Request (Request)
import Nacre.Request qualified as Request
import Nacre.Responses qualified as Responses
import Network.Wai qualified as Wai

type i --> o = (i, o)

data family Contract r

data family Function (ctx :: Type -> Type) r

data family Endpoint (ctx :: Type -> Type) r

data family Consumer r

data instance Contract (Request m p q b h --> o) where
    (:->) ::
        Request.Contract m p q b h ->
        Responses.Contract o ->
        Contract (Request m p q b h --> o)

(.->) ::
    (Request.Contract m p q b h -> Request.Contract m' p' q' b' h') ->
    (Responses.Contract o -> Responses.Contract o') ->
    (Contract (Request m p q b h --> o) -> Contract (Request m' p' q' b' h' --> o'))
(.->) reqF respF (reqC :-> respC) = reqF reqC :-> respF respC

data instance Server ctx (Request m p q b h --> o) where
    (:=) ::
        Contract (Request m p q b h --> o) ->
        ((Request.Data m p q b h, Wai.Request) -> ctx o) ->
        Server ctx (Request m p q b h --> o)

(.=) ::
    (Contract (Request m p q b h --> o) -> Contract (Request m' p' q' b' h' --> o')) ->
    (((Request.Data m p q b h, Wai.Request) -> ctx o) -> ((Request.Data m' p' q' b' h', Wai.Request) -> ctx o')) ->
    (Server ctx (Request m p q b h --> o) -> Server ctx (Request m' p' q' b' h' --> o'))
(.=) contractF handlerF (contract := hdlr) = contractF contract := handlerF hdlr

data instance Client (Request m p q b h --> o) where
    Client ::
        {fetch_ :: Request.Data m p q b h -> IO (Either ClientError o)} ->
        Client (Request m p q b h --> o)

data instance Handler ctx (Request m p q b h --> o) = Handler ((Request.Data m p q b h, Wai.Request) -> ctx o)

fn :: ((Request.Data m p q b h, Wai.Request) -> ctx o) -> Handler ctx (Request m p q b h --> o)
fn = Handler

data ClientError = ClientError
