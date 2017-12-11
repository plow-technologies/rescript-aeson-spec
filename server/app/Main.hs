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

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Proxy
import System.Directory (doesFileExist)
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs

import Test.Aeson.Internal.RandomSamples
import Test.Aeson.Internal.Utils

import Data.ByteString.Lazy (writeFile, readFile)
import Prelude hiding (readFile)

data Person =
  Person
    { name :: String
    , age :: Int
    } deriving (Eq,Show,Generic)

instance ToJSON Person
instance FromJSON Person
instance ToADTArbitrary Person
instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary


data Company =
  Company
    { companyName :: String
    , employees :: [Person]
    } deriving (Eq,Show,Generic)

instance ToJSON Company
instance FromJSON Company
instance ToADTArbitrary Company
instance Arbitrary Company where
  arbitrary = do
    k <- choose (0,2)
    ps <- vector k 
    Company <$> arbitrary <*> pure ps

data Shape
  = Square Int Int
  | Triangle Int Int Int
  | Rectangle Int Int Int Int
  deriving (Eq,Show,Generic)
  
instance ToJSON Shape
instance FromJSON Shape
instance ToADTArbitrary Shape
instance Arbitrary Shape where
  arbitrary =
    oneof
      [ Square <$> arbitrary <*> arbitrary
      , Triangle <$> arbitrary <*> arbitrary <*> arbitrary
      , Rectangle <$>  arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ]



type TestAPI = "person"  :> ReqBody '[JSON] Person  :> Post '[JSON] Person
          :<|> "company" :> ReqBody '[JSON] Company :> Post '[JSON] Company
          :<|> "shape" :> ReqBody '[JSON] Shape :> Post '[JSON] Shape
          :<|> "people"    :> ReqBody '[JSON] [Person]  :> Post '[JSON] [Person]
          :<|> "companies" :> ReqBody '[JSON] [Company] :> Post '[JSON] [Company]
          :<|> "shapes" :> ReqBody '[JSON] [Shape] :> Post '[JSON] [Shape]

testAPI :: Proxy TestAPI
testAPI = Proxy

server :: Server TestAPI
server = (\person -> do
             liftIO $ print "person route called"
             return person)
    :<|> (\company -> do
             liftIO $ print "company route called"
             return company)
    :<|> (\shape -> do
             liftIO $ print "shape route called"
             return shape)
    :<|> (\people -> do
             liftIO $ print "people route called"
             return people)
    :<|> (\companies -> do
             liftIO $ print "companies route called"
             return companies)
    :<|> (\shapes -> do
             liftIO $ print "shapes route called"
             return shapes)

app :: Application
app = serve testAPI server

main :: IO ()
main = do
  mkGoldenFileForType 100 (Proxy :: Proxy Person) "../__tests__/golden" 
  mkGoldenFileForType 100 (Proxy :: Proxy Company) "../__tests__/golden"
  mkGoldenFileForType 100 (Proxy :: Proxy Shape) "../__tests__/golden"
  run 8081 app
