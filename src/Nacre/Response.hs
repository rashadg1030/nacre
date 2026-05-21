module Nacre.Response where

import Data.Kind (Type)

data Response (f :: (Type -> Type -> Type) -> Type -> Type) s h b = Response
    { status_ :: f Status s
    , resHeaders_ :: f ResHeaders h
    , resBody_ :: f ResBody b
    }

{- | In case the user just wants to return the Raw Response. A Raw Response provides no
type information about the response, besides the structure provided by Wai.Response.
Might not need it honestly because you just use a Response with the base types for everything
to represent an "any" response
-}
data ResponseRaw (r :: ((Type -> Type -> Type) -> Type -> Type) -> Type)
    = ResponseRaw Wai.Response

response :: Response IsoContract HTTP.Status HTTP.ResponseHeaders LBS.ByteString
response = undefined

status ::
    IsoContract Status s ->
    ( Response IsoContract HTTP.Status h b ->
      Response IsoContract s h b
    )
status = undefined

resHeaders ::
    IsoContract ResHeaders h ->
    ( Response IsoContract s HTTP.ResponseHeaders b ->
      Response IsoContract s h b
    )
resHeaders = undefined

resBody ::
    IsoContract ResBody b ->
    ( Response IsoContract s h LBS.ByteString ->
      Response IsoContract s h b
    )
resBody = undefined
