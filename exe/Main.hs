{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import GHC.Generics (Generic)
import Servant
import Servant.Multipart
import Servant.JuicyPixels
import Codec.Picture
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Torch 
import Torch.Vision
import System.Environment

data Result = Result
  { msg :: String
  , result :: [Float]
  } deriving (Show, Generic) 

instance ToJSON Result
instance FromJSON Result

data Box = Box
  { classid :: Int
  , bx :: Float
  , by :: Float
  , bw :: Float
  , bh :: Float
  } deriving (Show) 

newtype BoundingBox = BoundingBox [Box] deriving (Show)
newtype Labels = Labels [String] deriving (Show)

type HelloTorchAPI = "compute2x" :> Capture "value" Float :> Get '[JSON] [Result]

type BoundingBoxAPI = "boundingbox" :> MultipartForm Mem (MultipartData Mem) :> Post '[JPEG 50] DynamicImage

type API = HelloTorchAPI
      :<|> BoundingBoxAPI

helloTorch :: MonadIO m => Float -> m [Result] 
helloTorch value = pure $ 
  [Result ("f(x) = 2.0 * x is " ++ show result' ++ " for x = " ++ show value) [result']]
  where
    t = asTensor value :: Tensor
    result' = asValue (2.0 * t) :: Float

toLabels :: BL.ByteString -> Handler Labels
toLabels str = do
  let ssv :: [String]
      ssv =
        map BLC.unpack $
        BL.split (fromIntegral $ fromEnum '\n') str
  return $ Labels ssv


toBoundingBox :: BL.ByteString -> Handler BoundingBox
toBoundingBox str = do
  let ssv =
        filter (\v -> v /= []) $
        map (BL.split (fromIntegral $ fromEnum ' ')) $
        BL.split (fromIntegral $ fromEnum '\n') str
  bbox <- forM ssv $ \v ->
    case map BLC.unpack v of
      (classid': x': y': w': h' :_) -> do
        case (readMaybe classid', readMaybe x', readMaybe y', readMaybe w', readMaybe h') of
          (Just cid, Just x, Just y, Just w, Just h) -> return $ Box cid x y w h
          _ -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ "1,Can not parse:" ++ show v}
      _ -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ "2,Can not parse:" ++ show ssv}
  return $ BoundingBox bbox

drawBoundingBox :: DynamicImage -> Labels -> BoundingBox -> Handler ()
drawBoundingBox img (Labels labels) (BoundingBox bbox) = do
  let input_image = convertRGB8 img
      width = imageWidth input_image
      height = imageHeight input_image
  forM_ bbox $ \dat -> do
    let cid = classid dat
        x = fromIntegral width * bx dat
        y = fromIntegral height * by dat
        w = fromIntegral width * bw dat
        h = fromIntegral height * bh dat
        x0 = x - w/2
        y0 = y - h/2
        x1 = x + w/2
        y1 = y + h/2
    liftIO $ do
      drawString (labels !! cid) (round x0+1) (round y0+1) (255,255,255) (0,0,0) input_image
      drawRect (round x0) (round y0) (round x1) (round y1) (255,255,255) input_image
  

imageTorch :: MultipartData Mem -> Handler DynamicImage
imageTorch multipartData = do
  img <-
    case lookupFile "image" multipartData of
      Right file -> do
        case decodeImage (toStrict $ fdPayload file) of
           Right img -> return img
           Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}
      Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}

  Labels labels <- case lookupFile "labels" multipartData of
    Right file -> toLabels (fdPayload file)
    Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}

  BoundingBox bbox <- case lookupFile "bbox" multipartData of
    Right file -> toBoundingBox (fdPayload file)
    Left err -> throwError $ err404 { errBody = fromStrict $ encodeUtf8 $ T.pack $ show err}

  let input_image = convertRGB8 img
      width = imageWidth input_image
      height = imageHeight input_image

  forM_ bbox $ \dat -> liftIO $ do
    let cid = classid dat
        x = fromIntegral width * bx dat
        y = fromIntegral height * by dat
        w = fromIntegral width * bw dat
        h = fromIntegral height * bh dat
        x0 = x - w/2
        y0 = y - h/2
        x1 = x + w/2
        y1 = y + h/2
        label = if cid >= length labels then "Unknown" else labels !! cid
    drawString label (round x0+1) (round y0+1) (255,255,255) (0,0,0) input_image
    drawRect (round x0) (round y0) (round x1) (round y1) (255,255,255) input_image

  return $ ImageRGB8 input_image

torchApi :: Proxy API
torchApi = Proxy

server :: Server API
server = helloTorch :<|> imageTorch

app :: Application
app = serve torchApi server

main :: IO ()
main = do
  mport <- lookupEnv "PORT"
  case mport of
    Just port -> do
      putStrLn $ "Running server on port " ++ port
      run (read port) app
    Nothing -> do
      let port = 8081
      putStrLn $ "Running server on port " ++ show port
      run port app
