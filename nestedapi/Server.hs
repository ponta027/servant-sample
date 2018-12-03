{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Server where

--import Control.Monad.Except as E
--import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Text 

import Servant.Server.Internal
--import Servant.Server.Internal.ServantErr
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class

data User = User { 
        name                :: String, 
        age                 :: Int, 
        email               :: String --,
        } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" --(fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         --(fromGregorian 1905 12 1)
  ]


type UserAPI3 = -- view the user with given userid, in JSON
                Capture "userid" Int :> Get '[JSON] User
                -- /userid/1
           :<|> -- delete the user with given userid. empty response
                Capture "userid" Int :> DeleteNoContent '[JSON] NoContent
           :<|> API1
           :<|> API2
           :<|> ProductsAPI
            -- 
--           :<|> API3

{-- --}
type API1 = "users" :>
  (    Get '[JSON] [User] -- user listing
  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
  )

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
  (    Get '[JSON] User -- just display the same user back, don't register it
  :<|> PostNoContent '[JSON] NoContent  -- register the user. empty response
  )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
  (    Get '[JSON] SecretData -- get some secret data, if authorized
  :<|> ReqBody '[JSON] SecretData :> PostNoContent '[JSON] NoContent -- add some secret data, if authorized
  )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString


type ProductsAPI =
    Get '[JSON] [Product]
    :<|> ReqBody '[JSON] Product    :> PostNoContent '[JSON] NoContent
    :<|> "product" :> Capture "productid" Int    :> 
        (   Get '[JSON] Product 
            :<|> ReqBody '[JSON] Product :> PutNoContent '[JSON] NoContent
            :<|> DeleteNoContent '[JSON] NoContent 
        )

data Product = Product { productId :: Int } deriving (Eq, Show, Generic)
instance ToJSON Product
instance FromJSON Product



productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
            where   getProducts :: Handler [Product]
                    getProducts = return ([Product 11 ])
                    newProduct :: Product -> Handler NoContent
                    newProduct = error "....."
                    productOperations productId = 
                        viewProduct productId :<|> updateProduct productId :<|> deleteProduct productId
                        where   viewProduct :: Int -> Handler Product
                                viewProduct a = return ((Product a))
                                updateProduct :: Int -> Product -> Handler NoContent
                                updateProduct = error "..."
                                deleteProduct :: Int -> Handler NoContent
                                deleteProduct = error "..."
{-- --}


server8 :: Server UserAPI3
server8 = getUser
        :<|> deleteUser
        :<|> (  getUsers           -- API1 
                :<|> getUser            -- API1
             )
        :<|> userOperation           -- API2
        :<|> productsServer
--        :<|> (secretdataOperation)                          -- API3
        where   getUser :: Int -> Handler User
                getUser _user_id = return (users1 !! 0)
                getUsers :: Handler [User]
                getUsers = return ( users1 )
                deleteUser :: Int -> Handler NoContent
                deleteUser _userid = error "..."
                userOperation userinfo =
                    viewUser userinfo :<|> updateUser userinfo
--                secretdataOperation header = 
--                    viewSecreteData header :<|> postSecreteData header 
                    where 
                        viewUser :: User -> Handler User
                        viewUser user = return user
                        updateUser :: User -> Handler NoContent
                        updateUser user = error "...."
--                        postSecreteData :: Maybe Text -> SecretData -> Handler NoContent
--                        postSecreteData header secretData = error "...."
--                        viewSecreteData :: Maybe Text-> Handler SecretData
--                        viewSecreteData header = return ( "Sample" ::SecretData)

api :: Proxy UserAPI3 
api = Proxy

app1 :: Application
app1 = serve api server8

startApp :: IO()
startApp = run 8081 app1


