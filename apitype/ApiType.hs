{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}


module ApiType where

{--
 refs:https://github.com/haskell-servant/example-servant-minimal/blob/master/src/App.hs
 --}

import           Control.Monad.Trans
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

{-- --}
import           Data.Time.Clock
--import Lucid
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text as T
import Servant.API.BasicAuth



{-- data --}
data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data User = User { 
        name                :: String, 
        age                 :: Int, 
        email               :: String --,
--        registration_data   :: UTCTime
        } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" --(fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         --(fromGregorian 1905 12 1)
  ]


{--
type RootEndPoint = :> Get '[JSON] [User]
 --}
 --
--data SortBy = Name | Age deriving (Eq, Show, Generic)
type SortBy = String
type Users = [User]

type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] Users
                :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI4 = "users" :> Get '[JSON] [User]
           :<|> "admins" :> Get '[JSON] [User]

type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

           :<|> "user" :> Capture "userid" Integer :> DeleteNoContent '[JSON] NoContent
                -- equivalent to 'DELETE /user/:userid'
                --
type UserAPI6   = "users" :> QueryParam "sortBy" SortBy:> Get '[JSON] [User]

type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
                -- - equivalent to 'POST /users' with a JSON object
                --   describing a User in the request body
                -- - returns a User encoded in JSON

           :<|> "users" :> Capture "userid" Integer
                        :> ReqBody '[JSON] User
                        :> Put '[JSON] User
                -- - equivalent to 'PUT /users/:userid' with a JSON
                --   object describing a User in the request body
                -- - returns a User encoded in JSON
                --
type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]
type UserAPI9 = "users9" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]
type UserAPI10 = "users10" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])
type UserAPI11 = BasicAuth "my-realm" User :> UserAPI2

type API=
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item
  :<|> UserAPI2
  :<|> UserAPI3
  :<|> UserAPI4
  :<|> UserAPI5
  :<|> UserAPI6
  :<|> UserAPI7
  :<|> UserAPI8
--  :<|> UserAPI9
-- T.B.D  :<|> UserAPI10
-- T.B.D  :<|> UserAPI11

{-- --}
itemApi :: Proxy API
itemApi = Proxy


server :: Server API
server = getItemById 
         :<|> serverAPI2
         :<|> serverAPI3
         :<|> serverAPI4
         :<|> serverAPI5
         :<|> serverAPI6
         :<|> serverAPI7
         :<|> serverAPI8
--         :<|> serverAPI9
-- T.B.D         :<|> serverAPI10
-- T.B.D         :<|> serverAPI11
--        where getUsers :: SortBy -> Handler [User]
--             getUsers sortby = return users1

serverAPI2 :: Server UserAPI2
serverAPI2 = getAllUsers
            :<|> getAllUsers

serverAPI3 :: Server UserAPI3
serverAPI3 = getAllUsers
            :<|> getAllUsers
getAllUsers ::  Handler [User]
getAllUsers = return users1


serverAPI4 :: Server UserAPI4
serverAPI4 = getAllUsers
            :<|> getAllUsers

serverAPI5 :: Server UserAPI5
serverAPI5 = getUserById
            :<|> deleteUserById
getUserById :: Integer -> Handler User
getUserById id = return ( users1 !! 0 )

deleteUserById :: Integer -> Handler NoContent
deleteUserById id = error "..."

serverAPI6 :: Server UserAPI6
serverAPI6 = getUsers


getItems :: Handler [Item]
getItems  = return [exampleItem]

getUsers :: Maybe SortBy -> Handler [User]
getUsers sortBy= return users1

serverAPI8 ::Maybe Text -> Handler [User]
serverAPI8 agent= return users1

serverAPI9 :: Handler [User]
serverAPI9 = return users1

-- T.B.D
serverAPI10 :: Handler [User]
serverAPI10 = return users1

{--
T.B.D
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (User "servant"))
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

serverAPI11 :: BasicAuth -> Handler [User]
serverAPI11 a = getAllUsers
            :<|> getAllUsers

--}

serverAPI7 :: Server UserAPI7
serverAPI7 = xxx
            :<|> yyy

xxx :: User -> Handler User
xxx user= return (users1 !! 0)

yyy :: Integer -> User -> Handler User
yyy sortBy user= return (users1 !! 0)




getItemById :: Integer -> Handler Item
getItemById x = return exampleItem


exampleItem :: Item
exampleItem = Item 0 "example item"

{----------------------------}
app1 :: Application
app1 =  serve itemApi server


startApp :: IO()
startApp = run 8081 app1



