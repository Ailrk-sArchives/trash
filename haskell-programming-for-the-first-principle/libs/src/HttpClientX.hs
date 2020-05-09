{-# LANGUAGE OverloadedStrings #-}
module HttpClientX where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import System.IO (stdout)
import Network.HTTP.Simple
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.Yaml as Yaml
import Data.Aeson (Value)
import qualified Data.Conduit.List as CL

httpclientRun:: IO ()
httpclientRun = do
  basic
  json
  buildreq
  buildbody
  catchFailedConnection
  streaming
  overrideproxy
  connManager

basic :: IO ()
basic = do
  response <- httpLBS "http://httpbin.org/get"
  putStrLn $ "The status code was: " ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response

json :: IO ()
json = do
  response <- httpJSON "http://httpbin.org/get"
  putStrLn $ "The status code was" ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

buildreq :: IO ()
buildreq = do
  request' <- parseRequest "POST http://httpbin.org/post"
  let request =
       setRequestMethod "PUT"
       $ setRequestPath "/put"
       $ setRequestQueryString [("hello", Just "world")]
       $ setRequestBodyLBS "This is my request body"
       $ setRequestSecure True
       $ setRequestPort 443 request'
  response <- httpJSON request
  putStrLn $ "The status code was" ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

data Person = Person String Int
instance ToJSON Person where
  toJSON (Person name age) = object
    [ "name" .= name
    , "age"  .= age
    ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

buildbody :: IO ()
buildbody = do
  let request = setRequestBodyJSON people "POST http://httpbin.org/post"
  response <- httpJSON request :: IO (Response Value)
  putStrLn $ "current status is " ++ show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  S8.putStrLn $ Yaml.encode (getResponseBody response)

catchFailedConnection :: IO ()
catchFailedConnection = do
  eresponse <- try $ httpLBS "http://does-not-exist"
  case eresponse of
    Left e -> print (e :: HttpException)
    Right response -> L8.putStrLn $ getResponseBody response

streaming :: IO ()
streaming = httpSink "http://httpbin.org/get" $ \response -> do
  liftIO $ putStrLn
         $ "The status code was: "
        ++ show (getResponseStatusCode response)
  CL.mapM_ (S8.hPut stdout)

overrideproxy :: IO ()
overrideproxy = do
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 3128)) "https://httpbin.org/get"
  eresponse <- try $ httpLBS request
  case eresponse of
    Left e -> print (e :: HttpException)
    Right response -> do
      putStrLn $ "The status code was" ++
        show (getResponseStatusCode response)
      L8.putStrLn $ getResponseBody response

connManager :: IO ()
connManager = do
  manager <- newManager tlsManagerSettings
  let request = setRequestManager manager "https://httpbin.org/get"
  response <- httpLBS request
  putStrLn $ "The status code was: " ++
    show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response




