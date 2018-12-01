module AWSLambda.Runtime.Invocation.Callback
  ( run
  ) where

import Prelude (id)
import Protolude

import Data.Default.Class (def)
import qualified Data.Text.Encoding as TextEncoding
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/:))

import AWSLambda.Runtime.Handler.Response (HandlerResponse(..))
import qualified AWSLambda.Runtime.Handler.Response as HandlerResponse
import AWSLambda.Runtime.Invocation.Internal

data Response =
  Response (Either ErrorCode ())

run :: (Text, Int) -> Text -> HandlerResponse -> IO ()
run (host, port) reqId handlerRsp@SuccessHandlerResponse {} = do
  let url =
        Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
        "response"
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp
run (host, port) reqId handlerRsp@FailureHandlerResponse {} = do
  let url =
        Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
        "error"
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp

doPost :: Req.Url scheme -> Int -> Text -> HandlerResponse -> IO Response
doPost url port reqId handlerRsp = do
  print "Posting callback..."
  Req.runReq def $ do
    let payload =
          TextEncoding.encodeUtf8 $ HandlerResponse.getPayload handlerRsp
    let contentTypeHeader =
          Req.header "content-type" $
          TextEncoding.encodeUtf8 $
          maybe "text/html" id (mContentType handlerRsp)
    let options = contentTypeHeader <> Req.port port
    rsp <-
      Req.req Req.POST url (Req.ReqBodyBs payload) Req.ignoreResponse options
    let code = Req.responseStatusCode rsp
    if not (code >= 200 && code <= 299)
      then do
        liftIO $
          print
            ("Failed to post handler success response. Http response code: " <>
             show code :: Text)
        return $ Response (Left (ErrorCode code))
      else do
        liftIO $
          print
            ("Success to post handler success response. Http response code: " <>
             show code :: Text)
        return $ Response (Right ())

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
