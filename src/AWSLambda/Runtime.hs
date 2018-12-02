{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Runtime
  ( runHandler
  , runHandler'
  , HandlerRequest
  , HandlerResponse
  , mkSuccessResponse
  , mkFailureResponse
  ) where

import Prelude (error)
import Protolude

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Logger
  ( Loc
  , LogLevel(..)
  , LogSource
  , LogStr
  , LoggingT(..)
  , MonadLogger
  , logDebug
  , logInfo
  , runNoLoggingT
  )
import Data.Default.Class (def)
import qualified Data.Text as Text
import qualified Network.HTTP.Req as Req
import System.Environment (lookupEnv)

import AWSLambda.Runtime.Handler.Request (HandlerRequest(..))
import qualified AWSLambda.Runtime.Handler.Request as HandlerRequest
import AWSLambda.Runtime.Handler.Response (HandlerResponse(..))
import qualified AWSLambda.Runtime.Handler.Response as HandlerResponse
import qualified AWSLambda.Runtime.Invocation.Callback as InvocationCallback
import qualified AWSLambda.Runtime.Invocation.Client as InvocationClient
import qualified AWSLambda.Runtime.Invocation.Next as NextInvocation
import System.IO (BufferMode(..), hSetBuffering)

defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

mkSuccessResponse :: Text -> Text -> HandlerResponse
mkSuccessResponse = HandlerResponse.mkSuccess

mkFailureResponse :: Text -> Text -> HandlerResponse
mkFailureResponse = HandlerResponse.mkFailure

runHandler :: (HandlerRequest -> IO HandlerResponse) -> IO ()
runHandler = runHandler' defaultLogLevel

runHandler' :: LogLevel -> (HandlerRequest -> IO HandlerResponse) -> IO ()
runHandler' logLevel handler = do
  hSetBuffering stdout LineBuffering
  runLoggingT (start handler) runLogs
  where
    runLogs :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    runLogs loc logSource logLevel' logStr
      | logLevel' >= logLevel = print logStr
      | otherwise = return ()

start ::
     (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
  => (HandlerRequest -> IO HandlerResponse)
  -> m ()
start handler = do
  $(logInfo) "Initialising the Haskell Lambda Runtime."
  ep <- getEndpoint
  either (error . Text.unpack) (loop handler) ep

getEndpoint ::
     (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
  => m (Either Text (Text, Int))
getEndpoint = do
  ep <- liftIO . lookupEnv $ "AWS_LAMBDA_RUNTIME_API"
  pure $
    maybe
      (Left "AWS_LAMBDA_RUNTIME_API not found in ENV")
      (Right . getHostAndPort . Text.pack)
      ep
  where
    getHostAndPort endpoint =
      let getPort rawPort = fromMaybe 80 . readMaybe . Text.unpack $ rawPort
       in case Text.splitOn ":" endpoint of
            [] -> ("", 80)
            [host] -> (host, 80)
            [host, portRaw] -> (host, getPort portRaw)
            host:portRaw:_ -> (host, getPort portRaw)

loop ::
     (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
  => (HandlerRequest -> IO HandlerResponse)
  -> (Text, Int)
  -> m ()
loop handler endpoint = do
  $(logDebug) "Getting next invocation..."
  NextInvocation.getWithRetries 3 endpoint >>= handleResponse
  where
    handleResponse (InvocationClient.SuccessResponse req) = do
      $(logDebug) "Invoking user handler."
      res <- liftIO . handler $ req
      $(logDebug) "Invoking user handler completed."
      InvocationCallback.run endpoint (requestId req) res
      loop handler endpoint
    handleResponse (InvocationClient.ErrorResponse code) =
      case code of
        InvocationClient.ErrorCode (-1) ->
          $(logDebug) "Failed to send HTTP request to retrieve next task."
        _ -> do
          $(logDebug)
            ("HTTP request was not successful. HTTP response code: " <>
             show code <>
             ". Retrying..")
          loop handler endpoint
