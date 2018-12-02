{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Runtime.Invocation.Next
  ( get
  , getWithRetries
  , Response(..)
  , ErrorCode(..)
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
import AWSLambda.Runtime.Invocation.Internal

newtype Response =
  Response (Either ErrorCode HandlerRequest)

getWithRetries ::
     forall m. (MonadThrow m, MonadCatch m, MonadLogger m, MonadIO m)
  => Int
  -> (Text, Int)
  -> m Response
getWithRetries maxRetries endpoint = getWithRetries' 0
  where
    getWithRetries' retryNum = do
      rsp <- try (get endpoint) :: m (Either SomeException Response)
      case rsp of
        Left ex ->
          if retryNum >= maxRetries
            then throwM ex
            else getWithRetries' (retryNum + 1)
        Right rsp' -> pure rsp'

get :: (MonadLogger m, MonadIO m) => (Text, Int) -> m Response
get (host, port) = do
  let url = Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: "next"
  rsp <-
    Req.runReq def $ do
      rsp <-
        Req.req
          Req.GET
          url
          Req.NoReqBody
          Req.bsResponse
          (Req.port port <> Req.responseTimeout 3000000)
      let code = Req.responseStatusCode rsp
      if not (code >= 200 && code <= 299)
        then pure $ Response (Left (ErrorCode code))
        else do
          let body = Req.responseBody rsp
          let mReqId = Req.responseHeader rsp "lambda-runtime-aws-request-id"
          case mReqId of
            Nothing -> pure $ Response (Left (ErrorCode (-1)))
            Just reqId ->
              pure $
              Response
                (Right
                   HandlerRequest
                     { payload = TextEncoding.decodeUtf8 body
                     , requestId = TextEncoding.decodeUtf8 reqId
                     , xrayTraceId = getHeader rsp "lambda-runtime-trace-id"
                     , clientContext =
                         getHeader rsp "lambda-runtime-client-context"
                     , cognitoIdentity =
                         getHeader rsp "lambda-runtime-cognito-identity"
                     , functionArn =
                         getHeader rsp "lambda-runtime-invoked-function-arn"
                     , deadline = getDeadline rsp
                     })
  case rsp of
    Response (Left (ErrorCode code)) -> do
      $(logDebug)
        ("Failed to get next invocation. Http Response code: " <> show code)
      pure rsp
    Response (Right req) -> do
      $(logDebug)
        ("Success to get next invocation. ReqId is" <> show (requestId req))
      pure rsp
  where
    getHeader r header = TextEncoding.decodeUtf8 <$> Req.responseHeader r header
    getDeadline r = do
      millis <-
        (Text.unpack <$> getHeader r "lambda-runtime-deadline-ms" >>= readMaybe) :: Maybe Int
      Just $ posixSecondsToUTCTime (fromIntegral millis / 1000)
