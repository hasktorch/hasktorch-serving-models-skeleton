{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception hiding (Handler)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Servant
import Servant.Server
import Servant.Multipart
import Servant.JuicyPixels
import Codec.Picture
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (isJust)
import qualified Data.Text as T


import Torch 
import System.Environment

data Result = Result {
  msg :: String, 
  result :: [Float]
  } deriving (Show, Generic) 

instance ToJSON Result
instance FromJSON Result

type HelloTorchAPI = "compute2x" :> Capture "value" Float :> Get '[JSON] [Result]

type BoundingBoxAPI = "boundingbox" :> MultipartForm Mem (MultipartData Mem) :> Post '[JPEG 50] DynamicImage

type API = HelloTorchAPI
      :<|> BoundingBoxAPI

helloTorch :: MonadIO m => Float -> m [Result] 
helloTorch value = pure $ 
  [Result ("f(x) = 2.0 * x is " ++ show result ++ " for x = " ++ show value) [result]]
  where
    t = asTensor value :: Tensor
    result = asValue (2.0 * t) :: Float

imageTorch :: MultipartData Mem -> Handler DynamicImage
imageTorch multipartData = do
  -- forM_ (inputs multipartData) $ \input -> do
  imgs <- forM (files multipartData) $ \file -> do
    if T.isSuffixOf "jpg" (fdFileName file) ||
       T.isSuffixOf "png" (fdFileName file) ||
       T.isSuffixOf "bmp" (fdFileName file) then do
      liftIO $ putStrLn $ "Content of " ++ show (fdFileName file)
      -- return (fdFileName file,fdPayload file)
      case decodeImage (toStrict $ fdPayload file) of
        Right img -> return (Just img)
        Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}
    else
      return Nothing
  labels <- forM (files multipartData) $ \file -> do
    if T.isSuffixOf "txt" (fdFileName file) ||
       T.isSuffixOf "csv" (fdFileName file) then do
      liftIO $ putStrLn $ "Content of " ++ show (fdFileName file)
      -- return (fdFileName file,fdPayload file)
      case decodeImage (toStrict $ fdPayload file) of
        Right img -> return (Just img)
        Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}
    else
      return Nothing
  case filter isJust imgs of
    (Just file):_ -> return file
    _ -> throwError $ err404 { errBody = "Not found an image file" }


torchApi :: Proxy API
torchApi = Proxy

server :: Server API
server = helloTorch :<|> imageTorch

app :: Application
app = serve torchApi server

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      mport <- lookupEnv "PORT"
      case mport of
        Just port -> do
          putStrLn $ "Running server on port " ++ port
          run (read port) app
        Nothing -> do
          let port = 8081
          putStrLn $ "Running server on port " ++ show port
          run port app
    port:_ -> do
      putStrLn $ "Running server on port " ++ port
      run (read port) app
