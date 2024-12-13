{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Server ( serverMain
              , rpc
              , broadcast
              , MsgHandler
              , IterationDecision(..) ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Reader
import           Data.Functor
import           Data.Text                          (pack)
import           Network.HTTP.Types                 (Status, status200, status404)
import           Network.Mime
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Requests
import           System.FilePath
import qualified Data.Aeson            as J
import qualified Data.ByteString       as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS        (unpack)
import qualified Data.ByteString.Lazy  as BSL
import qualified Network.WebSockets    as WS

newtype ConnectionState
  = ConnectionState
    { conn::WS.Connection }

data IterationDecision = Continue | Break

type MsgHandler = ReaderT ConnectionState IO

printErr :: Show a => a -> IO ()
printErr a = putStrLn $ "\ESC[31m" ++ show a ++ "\ESC[0m"

startWebsocketChannel
  :: MVar s
  -> (s -> FromMsg -> MsgHandler (IterationDecision, s))
  -> WS.PendingConnection
  -> IO ()
startWebsocketChannel userState handler pending = do
  conn <- WS.acceptRequest pending
  let state = ConnectionState conn
  WS.withPingThread conn 30 (return ()) $
    handle (disconnect conn) $ do
    fromMsg <- WS.receiveData conn
    case J.decode fromMsg::Maybe FromMsg of
      Just m -> do
        putStrLn "Opened websocket channel..."
        let f = modifyMVar_ userState $ \s -> runReaderT (handler s m) state >>=
                (\case
                    (Continue, s') -> s' <$ f
                    (Break,    s') -> pure s')
        f >> WS.sendClose conn BS.empty
          >> putStrLn "Websocket channel closed"
      Nothing -> putStrLn "Could not decode message from client. Disconnecting."

      where disconnect conn (e :: SomeException) = do
              WS.sendClose conn BS.empty
              putStrLn "Websocket channel closed due to a server exception!"
              printErr e

rpc :: (J.ToJSON a, J.FromJSON b) => a -> MsgHandler (Maybe b)
rpc msg = do
  state <- ask
  liftIO $ do
    let text = J.encode msg
    WS.sendTextData (conn state) text

  responseText <- liftIO $ do
    WS.receiveData (conn state)

  pure $ J.decode responseText

broadcast :: (J.ToJSON a) => a -> MsgHandler ()
broadcast msg = do
  state <- ask
  liftIO $ do
    let text = J.encode msg
    WS.sendTextData (conn state) text

serverMain :: s -> (s -> FromMsg -> MsgHandler (IterationDecision, s)) -> IO ()
serverMain initialState msgHandler = do
  userState <- newMVar initialState
  run 1234 (application userState)

  where application userState =
          websocketsOr WS.defaultConnectionOptions (wsApp userState) backupApp

        wsApp userState pendingConn = do
          startWebsocketChannel userState msgHandler pendingConn

        backupApp :: Application
        backupApp req respond = do
          response <-
            case rawPathInfo req of
              "/"   -> sendFile "./dist-client/index.html" status200
              route -> sendFile ("./dist-client" ++ BS.unpack route) status200
          respond response

sendFile :: FilePath -> Status -> IO Response
sendFile path status = do
  let mime = defaultMimeLookup (pack . snd . splitFileName $ path)
  (contents, status', mime') <- handle handler (BSL.readFile path <&> (,status,mime))
  let headers = [("Content-Type", mime')]
    in pure $ responseLBS status' headers contents

    where handler :: IOException -> IO (BSL.ByteString, Status, MimeType)
          handler = const (pure (_404, status404, "text/html"))

          _404 :: BSL.ByteString =
            "<!DOCTYPE html><html><body>404: Not Found</body></html>"
