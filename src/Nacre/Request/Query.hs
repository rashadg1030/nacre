module Nacre.Request.Query where

import qualified Network.HTTP.Types as HTTP

data Contract x a

any :: Contract HTTP.Query HTTP.Query
any = undefined

none :: Contract () ()
none = undefined
