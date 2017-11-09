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

type TestAPI = "person" :> ReqBody '[JSON] Person :> Post '[JSON] Person

testAPI :: Proxy TestAPI
testAPI = Proxy

server :: Server TestAPI
server = (\person -> do
             liftIO $ print "person route called"
             liftIO $ print person
             return person)

app :: Application
app = serve testAPI server

main :: IO ()
main = run 8081 app
