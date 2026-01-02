{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Nacre.Request where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Text (Text)
import Nacre.Request.Body qualified as Body
import Nacre.Request.Headers qualified as Headers
import Nacre.Request.Method qualified as Method
import Nacre.Request.Path qualified as Path
import Nacre.Request.Query qualified as Query
import Network.HTTP.Types qualified as HTTP

data Request m p q b h = Request
    { method_ :: m
    , path_ :: p
    , query_ :: q
    , requestBody_ :: b
    , requestHeaders_ :: h
    }

type Data m p q b h = Request m p q (IO b) h

type Contract m p q b h = Request (Method.Contract m m) (Path.Contract p p) (Query.Contract q q) (Body.Contract b b) (Headers.Contract h h)

request :: Contract HTTP.Method [Text] HTTP.Query BS.ByteString HTTP.RequestHeaders
request = Request Method.any Path.any Query.any Body.any Headers.any

method :: Method.Contract m m -> Contract HTTP.Method p q b h -> Contract m p q b h
method c = onMethod (const c)

path :: Path.Contract p p -> Contract m [Text] q b h -> Contract m p q b h
path c = onPath (const c)

query :: Query.Contract q q -> Contract m p HTTP.Query b h -> Contract m p q b h
query c = onQuery (const c)

requestBody :: Body.Contract b b -> Contract m p q BS.ByteString h -> Contract m p q b h
requestBody c = onRequestBody (const c)

requestHeaders :: Headers.Contract h h -> Contract m p q b HTTP.RequestHeaders -> Contract m p q b h
requestHeaders c = onRequestHeaders (const c)

onMethod :: (m -> m') -> Request m p q b h -> Request m' p q b h
onMethod f req = req { method_ = f (method_ req) }

onPath :: (p -> p') -> Request m p q b h -> Request m p' q b h
onPath f req = req { path_ = f (path_ req) }

onQuery :: (q -> q') -> Request m p q b h -> Request m p q' b h
onQuery f req = req { query_ = f (query_ req) }

onRequestBody :: (b -> b') -> Request m p q b h -> Request m p q b' h
onRequestBody f req = req { requestBody_ = f (requestBody_ req) }

onRequestHeaders :: (h -> h') -> Request m p q b h -> Request m p q b h'
onRequestHeaders f req = req { requestHeaders_ = f (requestHeaders_ req) }
