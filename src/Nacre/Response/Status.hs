module Nacre.Response.Status where

import qualified Network.HTTP.Types as HTTP

data Contract x a

any :: Contract HTTP.Status HTTP.Status
any = undefined
