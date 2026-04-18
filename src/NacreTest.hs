{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NacreTest where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Nacre.Function
import Nacre.Group (zipRecords, (=:=), (<$$>), (<:>), apply, Map(..))
import Nacre.Group qualified as Group
import Nacre.Request hiding (Contract)
import Nacre.Request qualified as Request
import Nacre.Response qualified as Response
import Nacre.Responses qualified as Responses
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

type GetUserFn = Request HTTP.Method () () ByteString HTTP.RequestHeaders --> ()

type CreateUserFn = Request HTTP.Method () () ByteString HTTP.RequestHeaders --> ()

getUserContract :: Contract GetUserFn
getUserContract = undefined

createUserContract :: Contract CreateUserFn
createUserContract = undefined

inlineServers = Group.do
    getUserContract := \(req, _) -> putStrLn "get user"
    createUserContract := \(req, _) -> putStrLn "create user"

contracts = Group.do
    getUserContract
    createUserContract

handlers = Group.do
    handler \(req, waiReq) -> do
        myFun req waiReq
        putStrLn "get user"
    handler \(req, waiReq) -> do
        myFun req waiReq
        putStrLn "create user"

-- myFun :: Data HTTP.Method () () ByteString HTTP.RequestHeaders -> Wai.Request -> IO ()
myFun = undefined

zippedServers = contracts =:= handlers

data MyApi f = MyApi
    { getUser_ :: f GetUserFn
    , createUser_ :: f CreateUserFn
    }
    deriving (Generic)

myContracts :: MyApi Contract
myContracts =
    MyApi
        { getUser_ = getUserContract
        , createUser_ = createUserContract
        }

myHandlers :: MyApi (Handler IO)
myHandlers =
    MyApi
        { getUser_ = handler \(req, waiReq) -> myFun req waiReq
        , createUser_ = handler \(req, waiReq) -> myFun req waiReq
        }

myServers :: MyApi (Server IO)
myServers = myContracts <:> myHandlers

data GetUserResponses
    = UserFound (Response.Response HTTP.Status ByteString HTTP.ResponseHeaders)
    | UserNotFound (Response.Response HTTP.Status ByteString HTTP.ResponseHeaders)
    deriving (Generic)

getUserResponses = Responses.responses
    GetUserResponses
    Response.response
    Response.response

type SuccessOrError =
    Either
        (Response.Response HTTP.Status ByteString HTTP.ResponseHeaders)
        (Response.Response HTTP.Status ByteString HTTP.ResponseHeaders)

successOrErrorContract :: Responses.Contract SuccessOrError
successOrErrorContract = Responses.either Response.response Response.response

data AuthToken = AuthToken ByteString

data AddAuth

instance Map AddAuth (Server ctx (Request m p q b h --> o)) where
    type With AddAuth (Server ctx (Request m p q b h --> o)) =
        Server ctx (Request m p q b (AuthToken, h) --> o)
    map _ = addAuthContract .-> id .= addAuthHandler
      where
        addAuthContract = undefined
        addAuthHandler originalHandler (reqData, waiReq) = undefined

authenticatedServers = AddAuth <$$> inlineServers

authenticatedServers' = apply @AddAuth inlineServers
