module Nacre.Request.Path where

import Data.Text (Text)

data Contract x a

any :: Contract [Text] [Text]
any = undefined

none :: Contract () ()
none = undefined
