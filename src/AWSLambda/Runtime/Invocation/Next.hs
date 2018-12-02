{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Runtime.Invocation.Next
  ( get
  , getWithRetries
  ) where

import Protolude hiding (get, try)

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, try)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Default.Class (def)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/:))

import AWSLambda.Runtime.Handler.Request (HandlerRequest(..))
import qualified AWSLambda.Runtime.Invocation.Client as InvocationClient

getWithRetries ::
     forall m. (MonadThrow m, MonadCatch m, MonadLogger m, MonadIO m)
  => Int
  -> (Text, Int)
  -> m (InvocationClient.Response HandlerRequest)
getWithRetries maxRetries endpoint = getWithRetries' 0
  where
    getWithRetries' retryNum = do
      rsp <-
        try (get endpoint) :: m (Either SomeException (InvocationClient.Response HandlerRequest))
      either handleError pure rsp
      where
        handleError ex
          | retryNum >= maxRetries = throwM ex
          | otherwise = getWithRetries' $ retryNum + 1

get ::
     (MonadLogger m, MonadIO m)
  => (Text, Int)
  -> m (InvocationClient.Response HandlerRequest)
get (host, port) = do
  let url = Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: "next"
  rsp' <-
    Req.runReq def $ do
      rsp <-
        Req.req
          Req.GET
          url
          Req.NoReqBody
          Req.bsResponse
          (Req.port port <> Req.responseTimeout 3000000)
      let code = Req.responseStatusCode rsp
      pure $
        if not (InvocationClient.isSuccessCode code)
          then InvocationClient.mkErrorResponse code
          else let buildResponse reqId =
                     InvocationClient.mkSuccessResponse
                       HandlerRequest
                         { payload =
                             TextEncoding.decodeUtf8 . Req.responseBody $ rsp
                         , requestId = reqId
                         , xrayTraceId = getHeader rsp "lambda-runtime-trace-id"
                         , clientContext =
                             getHeader rsp "lambda-runtime-client-context"
                         , cognitoIdentity =
                             getHeader rsp "lambda-runtime-cognito-identity"
                         , functionArn =
                             getHeader rsp "lambda-runtime-invoked-function-arn"
                         , deadline = getDeadline rsp
                         }
                in maybe
                     (InvocationClient.mkErrorResponse (-1))
                     buildResponse
                     (TextEncoding.decodeUtf8 <$>
                      Req.responseHeader rsp "lambda-runtime-aws-request-id")
  logResponse rsp'
  pure rsp'
  where
    logResponse (InvocationClient.SuccessResponse req) =
      $(logDebug)
        ("Success to get next invocation. ReqId is" <> (show . requestId $ req))
    logResponse (InvocationClient.ErrorResponse code) =
      $(logDebug)
        ("Failed to get next invocation. Http Response code: " <> show code)
    getHeader r header = TextEncoding.decodeUtf8 <$> Req.responseHeader r header
    getDeadline r = do
      millis <-
        (Text.unpack <$> getHeader r "lambda-runtime-deadline-ms" >>= readMaybe) :: Maybe Int
      Just $ posixSecondsToUTCTime (fromIntegral millis / 1000)
