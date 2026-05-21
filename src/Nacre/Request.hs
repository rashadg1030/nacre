module Nacre.Request where
import Data.Kind (Type)

data Request (f :: (Type -> Type -> Type) -> Type -> Type) m p q h b = Request
    { method_ :: f Method m
    , path_ :: f Path p
    , query_ :: f Query q
    , reqHeaders_ :: f ReqHeaders h
    , reqBody_ :: f ReqBody b
    }

request :: Request IsoContract HTTP.StdMethod [Text.Text] HTTP.Query HTTP.RequestHeaders LBS.ByteString
request = undefined

method ::
    IsoContract Method m ->
    ( Request IsoContract HTTP.StdMethod p q h b ->
      Request IsoContract m p q h b
    )
method = undefined

path ::
    IsoContract Path p ->
    ( Request IsoContract m [Text.Text] q h b ->
      Request IsoContract m p q h b
    )
path = undefined

query ::
    IsoContract Query q ->
    ( Request IsoContract m p HTTP.Query h b ->
      Request IsoContract m p q h b
    )
query = undefined

reqHeaders ::
    IsoContract ReqHeaders h ->
    ( Request IsoContract m p q HTTP.RequestHeaders b ->
      Request IsoContract m p q h b
    )
reqHeaders = undefined

reqBody ::
    IsoContract ReqBody b ->
    ( Request IsoContract m p q h LBS.ByteString ->
      Request IsoContract m p q h b
    )
reqBody = undefined
