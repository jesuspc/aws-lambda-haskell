{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad.Logger (MonadLogger, logDebug, logWarn)

data Response =
  Response (Either ErrorCode ())

run ::
     (MonadLogger m, MonadIO m)
  => (Text, Int)
  -> Text
  -> HandlerResponse
  -> m ()
run (host, port) reqId handlerRsp@SuccessHandlerResponse {} = do
  let url =
        Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
        "response"
  $(logDebug) ("Going to post callback to: " <> show url)
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp
run (host, port) reqId handlerRsp@FailureHandlerResponse {} = do
  let url =
        Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
        "error"
  $(logDebug) ("Going to post callback to: " <> show url)
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp

doPost ::
     (MonadLogger m, MonadIO m)
  => Req.Url scheme
  -> Int
  -> Text
  -> HandlerResponse
  -> m Response
doPost url port reqId handlerRsp = do
  $(logDebug) "Posting callback..."
  Req.runReq def $ do
    let payload =
          TextEncoding.encodeUtf8 $ HandlerResponse.getPayload handlerRsp
    let contentTypeHeader =
          Req.header "content-type" $
          TextEncoding.encodeUtf8 $
          maybe "text/html" id (mContentType handlerRsp)
    let options =
          contentTypeHeader <> Req.port port <> Req.responseTimeout 3000000
    rsp <-
      Req.req Req.POST url (Req.ReqBodyBs payload) Req.ignoreResponse options
    let code = Req.responseStatusCode rsp
    if not (code >= 200 && code <= 299)
      then pure $ Response (Left (ErrorCode code))
      else return $ Response (Right ())

handleResponse :: (MonadLogger m, MonadIO m) => Text -> Response -> m Bool
handleResponse reqId (Response (Right ())) = do
  $(logDebug) ("Success callback HTTP request for invocation " <> reqId)
  return True
-- TODO: Improve handling for this scenario by reporting it to the runtime/init/error endpoint
-- See https://github.com/awslabs/aws-lambda-rust-runtime/blob/ad28790312219fb63f26170ae0d8be697fc1f7f2/lambda-runtime/src/runtime.rs#L12 fail_init
-- Also see https://github.com/awslabs/aws-lambda-rust-runtime/blob/master/lambda-runtime-client/src/client.rs
handleResponse reqId (Response (Left (ErrorCode code))) = do
  $(logWarn)
    ("HTTP Request for invocation" <> reqId <>
     "was not successful. HTTP response code: " <>
     show code)
  return False
