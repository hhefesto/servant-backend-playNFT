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
        Just sd' = Map.lookup "signedData" myInputs
        Just sig' = Map.lookup "signature" myInputs
        sd :: String
        sd = show . unpack $ sd'
        sig :: String
        sig = "\"" <> unpack sig' <> "\""
    sess <- newSession defaultConfig
    x <- importMJS sess "./js/node_modules/web3/src/index.js"
    a :: JSVal <- eval sess (fromString sd) >>= evaluate
    b :: JSVal <- eval sess (fromString sig) >>= evaluate
    let rawJS = [js|
                   var Web3 = $x
                   web3 = new Web3.default("http://localhost:8545");
                   aux = web3.eth.accounts.recover($a,$b);
                   return aux;
                   |]
    res' :: EncodedString <- eval sess rawJS >>= evaluate

    let res = unEncodedString res'
    putStrLn "res:"
    print res
    killSession sess
  pure 0


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
