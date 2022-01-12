{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Foldable
import           Data.Map.Lazy                         as Map
import           Data.Solidity.Prim.Address            (Address, fromPubKey)
import           Data.String
import           Data.Text                             as Text
import           Data.Text.Encoding                    (encodeUtf8)
import           GHC.Generics
import           Language.JavaScript.Inline
import           Network.Ethereum.Api.Personal         (ecRecover)
import           Network.HTTP.Client                   hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Network.Socket                        (withSocketsDo)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart
import           System.Directory
import           System.IO
import           System.IO.Temp
import           System.Process

-- https://docs.servant.dev/en/stable/cookbook/file-upload/FileUpload.html

type API = "upload-image" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = do
  liftIO $ do
    let myInputs = fromList $ (\i -> (iName i, iValue i)) <$> (inputs multipartData)
    let Just sd = Map.lookup "signedData" myInputs
    let Just sig = Map.lookup "signature" myInputs
    putStrLn . Text.unpack $ "(signedData, signature) = (" <> sd <> ",,,,,,,,,,,,,, " <> sig <> ")"
    putStrLn $ "Signature size = " <> (show (Text.length sig))
    putStrLn "Inputs:"
    putStrLn $ show $ (inputs multipartData)
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content
  return 0

main :: IO ()
main = do
  putStrLn "Servant server running..."
  run 8081 (serve api upload)

-- main :: IO ()
-- main = withSocketsDo . bracket (forkIO startServer) killThread $ \_threadid -> do
--   -- we fork the server in a separate thread and send a test
--   -- request to it from the main thread.

test :: IO ()
test = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8081/"
  resp <- flip httpLbs manager =<< formDataBody form req
  print resp

  where form = [ partBS "title" "World"
               , partBS "text" $ encodeUtf8 "Hello"
               , partFileSource "file" "./README.md"
               -- , partFileSource "file" "./chamales.jpeg"
               ]

hello2 :: IO ()
hello2 = do
  sess <- newSession defaultConfig
  x <- importMJS sess "./js/node_modules/web3/src/index.js"
  -- x <- importMJS sess "./js/web3.min.js"
  let rawJS = [js|
                 var Web3 = $x
                 web3 = new Web3.default("http://localhost:8545");
                 console.error(typeof web3.eth.accounts.recover);
                 |]
  () <- eval sess rawJS >>= evaluate
  killSession sess
  pure ()
