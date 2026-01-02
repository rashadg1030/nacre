{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QualifiedDo #-}

module NacreTest where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Nacre.Function
import Nacre.Group (zipRecords, (=:=))
import Nacre.Group qualified as Group
import Nacre.Request hiding (Contract)
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

myFun :: Data HTTP.Method () () ByteString HTTP.RequestHeaders -> Wai.Request -> IO ()
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
myServers = zipRecords myContracts myHandlers
