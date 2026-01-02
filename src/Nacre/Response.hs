{-# LANGUAGE TypeFamilies #-}

module Nacre.Response where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Nacre.Response.Body qualified as Body
import Nacre.Response.Headers qualified as Headers
import Nacre.Response.Status qualified as Status
import Network.HTTP.Types qualified as HTTP

data Response s b h = Response
  { status_ :: s
  , responseBody_ :: b
  , responseHeaders_ :: h
  }

type Contract s b h = Response (Status.Contract s s) (Body.Contract b b) (Headers.Contract h h)

type Data s b h = Response s b h

response :: Contract HTTP.Status BS.ByteString HTTP.ResponseHeaders
response = Response Status.any Body.any Headers.any

status :: Status.Contract s s -> Contract HTTP.Status b h -> Contract s b h
status c = onStatus (const c)

responseBody :: Body.Contract b b -> Contract s BS.ByteString h -> Contract s b h
responseBody c = onResponseBody (const c)

responseHeaders :: Headers.Contract h h -> Contract s b HTTP.ResponseHeaders -> Contract s b h
responseHeaders c = onResponseHeaders (const c)

onStatus :: (s -> s') -> Response s b h -> Response s' b h
onStatus f res = res { status_ = f (status_ res) }

onResponseBody :: (b -> b') -> Response s b h -> Response s b' h
onResponseBody f res = res { responseBody_ = f (responseBody_ res) }

onResponseHeaders :: (h -> h') -> Response s b h -> Response s b h'
onResponseHeaders f res = res { responseHeaders_ = f (responseHeaders_ res) }
