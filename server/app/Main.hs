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

mk :: forall a. (Eq a, Show a, FromJSON a, ToJSON a, ToADTArbitrary a) => FilePath -> Proxy a -> IO ()
mk goldenPath Proxy = do
  (typeName, constructors) <- fmap (adtTypeName &&& adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
  mapM_
    (\constructor -> do
        let goldenFilePath = goldenPath <> "/" <> typeName <> ".json"
        exists <- doesFileExist goldenFilePath
        if exists
          then pure ()
          else createGoldenFile 100 constructor goldenFilePath
    ) constructors

data Person =
  Person
    { name :: String
    , age :: Int
    } deriving (Eq,Show,Generic)

instance ToJSON Person
instance FromJSON Person
instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary
instance ToADTArbitrary Person

data Company =
  Company
    { companyName :: String
    , employees :: [Person]
    } deriving (Eq,Show,Generic)

instance ToJSON Company
instance FromJSON Company
instance Arbitrary Company where
  arbitrary = do
    k <- choose (0,2)
    ps <- vector k 
    Company <$> arbitrary <*> pure ps
instance ToADTArbitrary Company

type TestAPI = "person"  :> ReqBody '[JSON] Person  :> Post '[JSON] Person
          :<|> "company" :> ReqBody '[JSON] Company :> Post '[JSON] Company
          :<|> "people"    :> ReqBody '[JSON] [Person]  :> Post '[JSON] [Person]
          :<|> "companies" :> ReqBody '[JSON] [Company] :> Post '[JSON] [Company]

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
    :<|> (\people -> do
             liftIO $ print "people route called"
             result <- liftIO $ cWithGolden "../__tests__/golden/Person.json" people
             liftIO $ print result
             return people)
    :<|> (\companies -> do
             liftIO $ print "companies route called"
             result <- liftIO $ cWithGolden "../__tests__/golden/Company.json" companies
             liftIO $ print result
             return companies)

cWithGolden :: forall a. (Show a, Eq a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  FilePath -> [a] -> IO Bool
cWithGolden goldenFile as = do
  eGoldenSamples <- eitherDecode' <$> readFile goldenFile
  case eGoldenSamples of
    Left _ -> return False
    Right goldenSamples -> return $ (samples goldenSamples) == as
  -- goldenSeed <- readSeed =<< readFile goldenFile
  -- sampleSize <- readSampleSize =<< readFile goldenFile
  -- newSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) goldenSeed
{-
compareWithGolden :: forall a. (Show a, Eq a, FromJSON a, ToJSON a, ToADTArbitrary a) =>
  String -> Maybe String -> String -> ConstructorArbitraryPair a -> FilePath -> IO ()
compareWithGolden topDir mModuleName typeName cap goldenFile = do
  goldenSeed <- readSeed =<< readFile goldenFile
  sampleSize <- readSampleSize =<< readFile goldenFile
  newSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: RandomSamples a <-
      either (throwIO . ErrorCall) return =<<
      A.eitherDecode' <$>
      readFile goldenFile
    newSamples `shouldBe` goldenSamples
  where
    whenFails :: forall b c. IO c -> IO b -> IO b
    whenFails = flip onException

    faultyFile = mkFaultyFilePath topDir mModuleName typeName cap

    writeComparisonFile newSamples = do
      writeFile faultyFile (encodePretty newSamples)
      putStrLn $
        "\n" ++
"INFO: Written the current encodings into " ++ faultyFile ++ "."
-}
app :: Application
app = serve testAPI server

main :: IO ()
main = do
  mk "../__tests__/golden" (Proxy :: Proxy Person)
  mk "../__tests__/golden" (Proxy :: Proxy Company)
  run 8081 app
