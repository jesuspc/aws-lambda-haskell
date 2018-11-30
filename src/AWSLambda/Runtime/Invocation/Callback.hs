module AWSLambda.Runtime.Invocation.Callback
  ( success
  , failure
  ) where

import Protolude

import AWSLambda.Runtime.Internal
import AWSLambda.Runtime.Invocation.Internal

data Response =
  Response (Either ErrorCode ())

success :: Text -> Text -> HandlerResponse -> IO ()
success endpoint reqId handlerRsp = do
  let url =
        endpoint <> "/2018-06-01/runtime/invocation/" <> reqId <> "/response"
  rsp <- doPost url reqId handlerRsp
  void $ handleResponse reqId rsp

failure :: Text -> Text -> HandlerResponse -> IO ()
failure endpoint reqId handlerRsp = do
  let url = endpoint <> "/2018-06-01/runtime/invocation/" <> reqId <> "/error"
  rsp <- doPost url reqId handlerRsp
  void $ handleResponse reqId rsp

doPost :: Text -> Text -> HandlerResponse -> IO Response
doPost url reqId handlerRsp = undefined

handleResponse :: Text -> Response -> IO Bool
handleResponse reqId (Response (Right ())) = return True
handleResponse reqId (Response (Left (ErrorCode code))) =
  if code == -1
    then do
      print $ "Failed to send HTTP request for invocation " <> reqId
      return False
    else do
      print $
        "HTTP Request for invocation" <> reqId <>
        "was not successful. HTTP response code: " <>
        show code
      return False
