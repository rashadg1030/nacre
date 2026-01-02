module Nacre.Request.Method where

import qualified Network.HTTP.Types as HTTP

data Contract x a

any :: Contract HTTP.Method HTTP.Method
any = undefined
