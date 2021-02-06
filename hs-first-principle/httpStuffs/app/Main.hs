module Main where

import Lib
import Network.Wreq
import qualified Data.ByteString.Lazy as BS

urls :: [String]
urls = [ "https://hvaccloud.org:5000/view/project/generic"
       , "https://hvaccloud.org:5000/view/2/spots"]

mappingGet :: [IO (Response BS.ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response BS.ByteString]
traversedUrls = traverse get urls


