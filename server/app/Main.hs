{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp

data Person =
  Person
    { name :: String
    , age :: Int
    } deriving (Eq,Show,Generic)

instance ToJSON Person
instance FromJSON Person

data Company =
  Company
    { companyName :: String
    , employees :: [Person]
    } deriving (Eq,Show,Generic)

instance ToJSON Company
instance FromJSON Company

type TestAPI = "person"  :> ReqBody '[JSON] Person  :> Post '[JSON] Person
          :<|> "company" :> ReqBody '[JSON] Company :> Post '[JSON] Company

testAPI :: Proxy TestAPI
testAPI = Proxy

server :: Server TestAPI
server = (\person -> do
             liftIO $ print "person route called"
             liftIO $ print person
             return person)
    :<|> (\company -> do
             liftIO $ print "company route called"
             liftIO $ print company
             return company)

app :: Application
app = serve testAPI server

main :: IO ()
main = run 8081 app
