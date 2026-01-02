module Nacre.Server where

import Nacre.Function

class AsServer t where
    serve :: t -> Application

instance AsServer (Server ctx (Request m p q b h --> o)) where

instance AsServer (s1 :<> s2) where
