module HttpStuff where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = Prelude.map get urls

traversedurls :: IO [Response ByteString]
traversedurls = traverse get urls
