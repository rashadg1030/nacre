module Nacre.Response.Body where

import qualified Data.ByteString as BS

data Contract x a

any :: Contract BS.ByteString BS.ByteString
any = undefined

none :: Contract () ()
none = undefined
