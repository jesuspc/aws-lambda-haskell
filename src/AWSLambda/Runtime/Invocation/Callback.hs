module AWSLambda.Runtime.Invocation.Callback
  ( run
  ) where

import Prelude (id)
import Protolude

import Data.Default.Class (def)
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Req as Req

import AWSLambda.Runtime.Internal
import AWSLambda.Runtime.Invocation.Internal

data Response =
  Response (Either ErrorCode ())

run :: Text -> Text -> HandlerResponse -> IO ()
run endpoint reqId handlerRsp@SuccessHandlerResponse {} = do
  let url =
        endpoint <> "/2018-06-01/runtime/invocation/" <> reqId <> "/response"
  rsp <- doPost url reqId handlerRsp
  void $ handleResponse reqId rsp
run endpoint reqId handlerRsp@FailureHandlerResponse {} = do
  let url = endpoint <> "/2018-06-01/runtime/invocation/" <> reqId <> "/error"
  rsp <- doPost url reqId handlerRsp
  void $ handleResponse reqId rsp

doPost :: Text -> Text -> HandlerResponse -> IO Response
doPost url reqId handlerRsp =
  Req.runReq def $ do
    let payload = TextEncoding.encodeUtf8 $ getPayload handlerRsp
    let contentTypeHeader =
          Req.header "content-type" $
          TextEncoding.encodeUtf8 $
          maybe "text/html" id (mContentType handlerRsp)
    let options = contentTypeHeader
    rsp <-
      Req.req
        Req.POST
        (Req.http url)
        (Req.ReqBodyBs payload)
        Req.ignoreResponse
        options
    let code = Req.responseStatusCode rsp
    if not (code >= 200 && code <= 299)
      then do
        liftIO $
          print
            ("Failed to post handler success response. Http response code: " <>
             show code :: Text)
        return $ Response (Left (ErrorCode code))
      else return $ Response (Right ())

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
