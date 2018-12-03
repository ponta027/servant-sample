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

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" --(fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         --(fromGregorian 1905 12 1)
  ]



server :: Server MyHandler
server = return $ addHeader 1797 $ users1 !! 0 


api :: Proxy MyHandler
api = Proxy

app1 :: Application
app1 = serve api server

startApp :: IO()
startApp = run 8081 app1


