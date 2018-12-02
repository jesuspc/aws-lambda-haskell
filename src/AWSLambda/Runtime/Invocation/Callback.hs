{-# LANGUAGE DataKinds #-}
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
import AWSLambda.Runtime.Invocation.Client as InvocationClient
import Control.Monad.Logger (MonadLogger, logDebug, logWarn)

getUrl :: Text -> Text -> HandlerResponse -> Req.Url 'Req.Http
getUrl host reqId SuccessHandlerResponse {} =
  Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
  "response"
getUrl host reqId FailureHandlerResponse {} =
  Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /: "error"

run ::
     (MonadLogger m, MonadIO m)
  => (Text, Int)
  -> Text
  -> HandlerResponse
  -> m ()
run (host, port) reqId handlerRsp = do
  let url = getUrl host reqId handlerRsp
  $(logDebug) ("Going to post callback to: " <> show url)
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp
  where
    handleResponse reqId (InvocationClient.SuccessResponse _) = do
      $(logDebug) ("Success callback HTTP request for invocation " <> reqId)
      return True
    -- TODO: Improve handling for this scenario by reporting it to the runtime/init/error endpoint
    -- See https://github.com/awslabs/aws-lambda-rust-runtime/blob/ad28790312219fb63f26170ae0d8be697fc1f7f2/lambda-runtime/src/runtime.rs#L12 fail_init
    -- Also see https://github.com/awslabs/aws-lambda-rust-runtime/blob/master/lambda-runtime-client/src/client.rs
    handleResponse reqId (InvocationClient.ErrorResponse code) = do
      $(logWarn)
        ("HTTP Request for invocation" <> reqId <>
         "was not successful. HTTP response code: " <>
         show code)
      return False

doPost ::
     (MonadLogger m, MonadIO m)
  => Req.Url scheme
  -> Int
  -> Text
  -> HandlerResponse
  -> m (InvocationClient.Response ())
doPost url port reqId handlerRsp = do
  $(logDebug) "Posting callback..."
  Req.runReq def $ do
    let payload =
          TextEncoding.encodeUtf8 . HandlerResponse.getPayload $ handlerRsp
    let contentTypeHeader =
          Req.header "content-type" $
          TextEncoding.encodeUtf8 $
          maybe "text/html" id (mContentType handlerRsp)
    let options =
          contentTypeHeader <> Req.port port <> Req.responseTimeout 3000000
    rsp <-
      Req.req Req.POST url (Req.ReqBodyBs payload) Req.ignoreResponse options
    let code = Req.responseStatusCode rsp
    pure $
      if not (InvocationClient.isSuccessCode code)
        then InvocationClient.mkErrorResponse code
        else InvocationClient.mkSuccessResponse ()
