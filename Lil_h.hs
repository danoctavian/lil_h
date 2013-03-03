{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Data.ByteString.Char8 as Ch8
import Data.ByteString as DatB

import System.FilePath.Posix 
import System.Environment
import System.Directory
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString as BS
import Control.Concurrent
import Data.List as L
import Control.Monad.Maybe
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad as CM

import Utils

data Config = Config {rootPath :: FilePath}
data ReqType = GET | POST -- bla bla
data Request = Request {reqType :: ReqType, reqPath :: FilePath}
 
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5004
  loop (Config "./files/") sock
 
loop config sock = do
  (conn, _) <- accept sock
  forkIO $ handleReq config conn 
  loop config sock

handleReq config conn = do
  reqText <- BS.recv conn 1024
  resp <- makeResponse config (parseReq reqText)
  sendAll conn resp
  sClose conn

parseReq txt = do
  let chunks = Ch8.words txt in do
   when (not (L.length chunks >= 2)) Nothing
   Just (Request GET (Ch8.unpack $ chunks !! 1))

makeResponse conf mreq = do
  case mreq of 
    Nothing -> return "invalid request"
    Just req -> do
      static <- runErrorT $ getStatic (rootPath conf ++ reqPath req) 
      case static of 
        Left error -> return . Ch8.pack $ error
        Right res -> return $ DatB.concat [okMessage,res]

getStatic path = do
  when (not $ isValidPath path) (throwError "path invalid")
  exists <- liftIO $ doesFileExist path
  when (not exists) (throwError "no file present")
  liftIO $ DatB.readFile path  

isValidPath p = not $ L.isInfixOf ".." p

okMessage :: ByteString
okMessage = "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\n"
