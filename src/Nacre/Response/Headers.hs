module Nacre.Response.Headers where

import qualified Network.HTTP.Types as HTTP

data Contract x a

any :: Contract HTTP.ResponseHeaders HTTP.ResponseHeaders
any = undefined

none :: Contract () ()
none = undefined
