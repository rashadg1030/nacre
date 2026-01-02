module Nacre.Request.Headers where

import qualified Network.HTTP.Types as HTTP

data Contract x a

any :: Contract HTTP.RequestHeaders HTTP.RequestHeaders
any = undefined

none :: Contract () ()
none = undefined
