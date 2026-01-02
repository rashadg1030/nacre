module Nacre.Request.Body where

import Data.ByteString qualified as BS

data Contract x a

any :: Contract BS.ByteString BS.ByteString
any = undefined

none :: Contract () ()
none = undefined
