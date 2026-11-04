{-# LANGUAGE OverloadedStrings #-}

module GetInputs where -- (downloadNotes, submitAnswer) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Applicative (liftA2)
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad (forM_, unless)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  ( BlockCipher (cbcDecrypt),
    Cipher (cipherInit),
    IV,
    makeIV,
  )
import Crypto.Data.Padding (Format (PKCS7), unpad)
import Crypto.Error (throwCryptoError)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens
  ( AsNumber (_Integer),
    AsValue (_String),
    key,
  )
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.Maybe (mapMaybe)
import Data.Text (Text (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ECSolution (createPath)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Text.Printf (printf)

downloadNotes :: Int -> Int -> IO ()
downloadNotes event quest = do
  Just seed <- getSeed
  keys <- getAESKeys event quest
  notes <- getNotes event quest (fromIntegral seed)

  let kn = mapMaybe decryptNote $ zipWith (liftA2 (,)) keys notes

  forM_ [1 ..] \part -> do
    forM_ ["sample", "actual"] \inputType -> do
      let filename = createPath inputType event quest part
      fileExists <- doesFileExist filename
      unless fileExists do
        putStrLn $ " * Creating file " ++ filename
        writeFile filename ""

  putStrLn $ "Downloading inputs for " ++ show event ++ " quest " ++ show quest
  forM_ (zip [1 ..] kn) $ \(part, note) -> do
    let filenameActual = createPath "actual" event quest part
    writeFile filenameActual (T.unpack note)
    putStrLn $ " * Downloaded part " ++ show part

  putStrLn "Done."

getOpts :: IO Options
getOpts = do
  loadFile defaultConfig
  cookie <- B.pack <$> getEnv "EC_COOKIE"
  pure $ defaults & header "Cookie" .~ ["everybody-codes=" <> cookie]

getSeed :: IO (Maybe Integer)
getSeed = do
  opts <- getOpts
  res <- getWith opts "https://api.everybody.codes/user/me"
  pure $ res ^? responseBody . key "seed" . _Integer

getAESKeys :: Int -> Int -> IO [Maybe Text]
getAESKeys event quest = do
  let uri = printf "https://api.everybody.codes/event/%d/quest/%d" event quest
  opts <- getOpts
  res <- getWith opts uri
  pure $ map (\k -> res ^? responseBody . key k . _String) ["key1", "key2", "key3"]

getNotes :: Int -> Int -> Int -> IO [Maybe Text]
getNotes event quest seed = do
  let uri = printf "https://everybody.codes/assets/%d/%d/input/%d.json" event quest seed
  opts <- getOpts
  res <- getWith opts uri
  pure $ map (\k -> res ^? responseBody . key k . _String) ["1", "2", "3"]

decryptNote :: Maybe (Text, Text) -> Maybe Text
decryptNote Nothing = Nothing
decryptNote (Just (k, n)) = case unpad (PKCS7 16) padded of
  Just x -> Just $ TE.decodeUtf8 x
  Nothing -> error "Failed to unpad"
  where
    kb = TE.encodeUtf8 k
    Right nb = B16.decode $ TE.encodeUtf8 n
    iv :: IV AES256
    Just iv = makeIV $ B.take 16 kb
    cipher :: AES256
    cipher = throwCryptoError $ cipherInit kb
    padded = cbcDecrypt cipher iv nb

submitAnswer :: Int -> Int -> Int -> String -> IO ()
submitAnswer event quest part result = do
  let uri = printf "https://everybody.codes/api/event/%d/quest/%d/part/%d/answer" event quest part
  opts <- getOpts
  res <- try $ postWith opts uri (object ["answer" .= result])

  case res of
    Left err -> do
      case err of
        HttpExceptionRequest _ (StatusCodeException resp _) -> do
          let status = resp ^. responseStatus
          case status ^. statusCode of
            409 -> putStrLn "Answer already submitted."
            423 -> putStrLn "Submission locked. Try again later."
            x -> do
              let msg = status ^. statusMessage
              putStrLn $ printf "Error submitting answer: HTTP %d [%s]" x $ B.unpack msg
        _ -> putStrLn $ "Unknown error: " ++ show err
    Right res -> putStrLn "Success"
