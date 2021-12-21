{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text.Encoding                    (encodeUtf8)
import           Network.HTTP.Client                   hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Network.Socket                        (withSocketsDo)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart

import qualified Data.ByteString.Lazy                  as LBS

-- https://docs.servant.dev/en/stable/cookbook/file-upload/FileUpload.html

type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
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
  run 8080 (serve api upload)

-- main :: IO ()
-- main = run 8080 (serve api upload)

-- main :: IO ()
-- main = withSocketsDo . bracket (forkIO startServer) killThread $ \_threadid -> do
--   -- we fork the server in a separate thread and send a test
--   -- request to it from the main thread.
test :: IO ()
test = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/"
  resp <- flip httpLbs manager =<< formDataBody form req
  print resp

  where form = [ partBS "title" "World"
               , partBS "text" $ encodeUtf8 "Hello"
               , partFileSource "file" "./README.md"
               -- , partFileSource "file" "./chamales.jpeg"
               ]
