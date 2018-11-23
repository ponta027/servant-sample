{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Lucid
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text as T


{--
type RootEndPoint = :> Get '[JSON] [User]
 --}
 --
--data SortBy = Name | Age deriving (Eq, Show, Generic)
type SortBy = String

type API=
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item
  :<|>
  "users" :> QueryParam "sortBy" SortBy:> Get '[JSON] [User]


{-- data --}
data User = User { 
        name                :: String, 
        age                 :: Int, 
        email               :: String --,
--        registration_data   :: UTCTime
        } deriving (Eq, Show, Generic)
instance ToJSON User
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" --(fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         --(fromGregorian 1905 12 1)
  ]


{-- --}
itemApi :: Proxy API
itemApi = Proxy


server :: Server API
server = getItemById 
         :<|> getUsers
--        where getUsers :: SortBy -> Handler [User]
--             getUsers sortby = return users1


getItems :: Handler [Item]
getItems  = return [exampleItem]

getUsers :: Maybe SortBy -> Handler [User]
getUsers sortBy= return users1

getItemById :: Integer -> Handler Item
getItemById x = return exampleItem


exampleItem :: Item
exampleItem = Item 0 "example item"

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool

app1 :: Application
app1 =  serve itemApi server


startApp :: IO()
startApp = run 8081 app1



