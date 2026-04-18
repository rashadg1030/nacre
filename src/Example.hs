{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nacre.Function
import Nacre.Group (zipRecords, (<$$>), (=:=), Map(..))
import Nacre.Group qualified as Group
import Nacre.Request hiding (Contract)
import Network.HTTP.Types qualified as HTTP

newtype UserId = UserId Int
newtype PostId = PostId Int
newtype CommentId = CommentId Int

data User = User {id_ :: UserId, name :: Text}
data Post = Post {postId :: PostId, title :: Text, body :: Text}
data Comment = Comment {commentId :: CommentId, text :: Text}

data AuthToken = AuthToken ByteString

type ListUsers = Request HTTP.Method () () () HTTP.RequestHeaders --> [User]
type GetUser = Request HTTP.Method UserId () () HTTP.RequestHeaders --> User
type ListPosts = Request HTTP.Method () () () HTTP.RequestHeaders --> [Post]
type GetPost = Request HTTP.Method PostId () () HTTP.RequestHeaders --> Post
type ListComments = Request HTTP.Method PostId () () HTTP.RequestHeaders --> [Comment]
type GetComment = Request HTTP.Method CommentId () () HTTP.RequestHeaders --> Comment
type SearchPosts = Request HTTP.Method () Text () HTTP.RequestHeaders --> [Post]

type CreateUser = Request HTTP.Method () () User HTTP.RequestHeaders --> User
type DeleteUser = Request HTTP.Method UserId () () HTTP.RequestHeaders --> ()
type CreatePost = Request HTTP.Method () () Post HTTP.RequestHeaders --> Post
type DeletePost = Request HTTP.Method PostId () () HTTP.RequestHeaders --> ()
type CreateComment = Request HTTP.Method PostId () Comment HTTP.RequestHeaders --> Comment

listUsersContract = undefined :: Contract ListUsers
getUserContract = undefined :: Contract GetUser
listPostsContract = undefined :: Contract ListPosts
getPostContract = undefined :: Contract GetPost
listCommentsContract = undefined :: Contract ListComments
getCommentContract = undefined :: Contract GetComment
searchPostsContract = undefined :: Contract SearchPosts

createUserContract = undefined :: Contract CreateUser
deleteUserContract = undefined :: Contract DeleteUser
createPostContract = undefined :: Contract CreatePost
deletePostContract = undefined :: Contract DeletePost
createCommentContract = undefined :: Contract CreateComment

listUsersHandler :: Handler IO ListUsers
listUsersHandler = handler \_ -> putStrLn "listing users" >> return []

getUserHandler :: Handler IO GetUser
getUserHandler = handler \_ -> putStrLn "getting user" >> return (User (UserId 1) "Alice")

listPostsHandler :: Handler IO ListPosts
listPostsHandler = handler \_ -> putStrLn "listing posts" >> return []

getPostHandler :: Handler IO GetPost
getPostHandler = handler \_ -> putStrLn "getting post" >> return (Post (PostId 1) "Hello" "World")

listCommentsHandler :: Handler IO ListComments
listCommentsHandler = handler \_ -> putStrLn "listing comments" >> return []

getCommentHandler :: Handler IO GetComment
getCommentHandler = handler \_ -> putStrLn "getting comment" >> return (Comment (CommentId 1) "Nice!")

searchPostsHandler :: Handler IO SearchPosts
searchPostsHandler = handler \_ -> putStrLn "searching posts" >> return []

createUserHandler :: Handler IO CreateUser
createUserHandler = handler \_ -> putStrLn "creating user" >> return (User (UserId 2) "Bob")

deleteUserHandler :: Handler IO DeleteUser
deleteUserHandler = handler \_ -> putStrLn "deleting user"

createPostHandler :: Handler IO CreatePost
createPostHandler = handler \_ -> putStrLn "creating post" >> return (Post (PostId 2) "New" "Post")

deletePostHandler :: Handler IO DeletePost
deletePostHandler = handler \_ -> putStrLn "deleting post"

createCommentHandler :: Handler IO CreateComment
createCommentHandler = handler \_ -> putStrLn "creating comment" >> return (Comment (CommentId 2) "Thanks")

publicContracts = Group.do
    listUsersContract
    getUserContract
    listPostsContract
    getPostContract
    listCommentsContract
    getCommentContract
    searchPostsContract

publicHandlers = Group.do
    listUsersHandler
    getUserHandler
    listPostsHandler
    getPostHandler
    listCommentsHandler
    getCommentHandler
    searchPostsHandler

publicServers = publicContracts =:= publicHandlers

authContracts = Group.do
    createUserContract
    deleteUserContract
    createPostContract
    deletePostContract
    createCommentContract

authHandlers = Group.do
    createUserHandler
    deleteUserHandler
    createPostHandler
    deletePostHandler
    createCommentHandler

authServers = authContracts =:= authHandlers

data AddAuth

instance Map AddAuth (Server ctx (Request m p q b h --> o)) where
    type
        With AddAuth (Server ctx (Request m p q b h --> o)) =
            Server ctx (Request m p q b (AuthToken, h) --> o)
    map _ = addAuthContract .-> id .= addAuthHandler
      where
        addAuthContract = undefined
        addAuthHandler _ = undefined

protectedServers = AddAuth <$$> authServers

allServers = Group.do
    publicServers
    protectedServers

data Api f = Api
    { listUsers :: f ListUsers
    , getUser :: f GetUser
    , listPosts :: f ListPosts
    , getPost :: f GetPost
    , listComments :: f ListComments
    , getComment :: f GetComment
    , searchPosts :: f SearchPosts
    , createUser :: f CreateUser
    , deleteUser :: f DeleteUser
    , createPost :: f CreatePost
    , deletePost :: f DeletePost
    , createComment :: f CreateComment
    }
    deriving (Generic)

apiContracts =
    Api
        { listUsers = listUsersContract
        , getUser = getUserContract
        , listPosts = listPostsContract
        , getPost = getPostContract
        , listComments = listCommentsContract
        , getComment = getCommentContract
        , searchPosts = searchPostsContract
        , createUser = createUserContract
        , deleteUser = deleteUserContract
        , createPost = createPostContract
        , deletePost = deletePostContract
        , createComment = createCommentContract
        }

apiHandlers =
    Api
        { listUsers = listUsersHandler
        , getUser = getUserHandler
        , listPosts = listPostsHandler
        , getPost = getPostHandler
        , listComments = listCommentsHandler
        , getComment = getCommentHandler
        , searchPosts = searchPostsHandler
        , createUser = createUserHandler
        , deleteUser = deleteUserHandler
        , createPost = createPostHandler
        , deletePost = deletePostHandler
        , createComment = createCommentHandler
        }

apiServers :: Api (Server IO)
apiServers = zipRecords apiContracts apiHandlers
