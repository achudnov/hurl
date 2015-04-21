{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Network.URI
import Network.HTTP
import Network.Browser
import Network.HTTP.Encoding
import System.Console.CmdArgs
import Data.Maybe
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (unless, when)
import System.IO

data Command = Command {url :: String
                       ,decodeBodies :: Bool
                       ,onlyBody :: Bool
                       ,file :: Maybe String}
               deriving (Data, Typeable)

myopts = Command {url = def &= args &= typ "URL"
                 ,decodeBodies = False
                 ,onlyBody     = False
                 ,file = def} 
         &= summary "HURL, CURL's little brother in Haskell"
         &= program "hurl"

main :: IO ()
main = do cmd <- cmdArgs myopts
          case parseURI $ url cmd of    
            Nothing -> putStrLn "Illegal URL syntax"
            Just uri -> download uri cmd
                        
download :: URI -> Command -> IO ()
download uri cmd = 
  let headers = 
        maybeToList (Header HdrHost <$> uriRegName <$> uriAuthority uri) ++
        [Header HdrAcceptEncoding "gzip, deflate",
         Header HdrAcceptCharset "ISO-8859-1,utf-8",
         Header HdrUserAgent "Mozilla/5.0 (Ubuntu; X11; Linux x86; rv:9.0.1)\
                              \ Gecko/20100101 Firefox/9.0.1"]
      rq = Request uri GET headers LBS.empty
  in browse (setAllowRedirects True >> request rq) >>= \(_, rsp) ->
  if decodeBodies cmd then case decode rsp of
    Left  err  -> putStrLn $ "Can't decode response body: " ++ show err
    Right (enc, drsp) -> printResponseS cmd drsp
  else printResponseLBS cmd rsp

printResponseS :: Command -> Response String -> IO ()
printResponseS cmd rsp@(Response _ _ _ body) =
  let ob = onlyBody cmd
  in do h <- maybe (return stdout) (`openBinaryFile` WriteMode) (file cmd)
        unless ob $ hPrint h rsp
        hPutStr h body
        when (isJust $ file cmd) (hClose h)
  
printResponseLBS :: Command -> Response LBS.ByteString -> IO ()
printResponseLBS cmd rsp@(Response _ _ _ body) =
  let ob = onlyBody cmd
  in do h <- maybe (return stdout) (`openBinaryFile` WriteMode) (file cmd)
        unless ob $ hPrint h rsp
        LBS.hPutStr h body
        when (isJust $ file cmd) (hClose h)  
  
