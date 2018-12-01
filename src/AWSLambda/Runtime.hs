module AWSLambda.Runtime
  ( runHandler
  , HandlerRequest
  , HandlerResponse
  , mkSuccessResponse
  , mkFailureResponse
  ) where

import Prelude (error)
import Protolude

import Data.Default.Class (def)
import qualified Data.Text as Text
import qualified Network.HTTP.Req as Req
import System.Environment (lookupEnv)

import AWSLambda.Runtime.Handler.Request (HandlerRequest(..))
import qualified AWSLambda.Runtime.Handler.Request as HandlerRequest
import AWSLambda.Runtime.Handler.Response (HandlerResponse(..))
import qualified AWSLambda.Runtime.Handler.Response as HandlerResponse
import qualified AWSLambda.Runtime.Invocation.Callback as InvocationCallback
import qualified AWSLambda.Runtime.Invocation.Next as NextInvocation
import System.IO (BufferMode(..), hSetBuffering)

mkSuccessResponse :: Text -> Text -> HandlerResponse
mkSuccessResponse p ct =
  SuccessHandlerResponse {mPayload = p, mContentType = Just ct}

mkFailureResponse :: Text -> Text -> HandlerResponse
mkFailureResponse errorMsg errorType =
  FailureHandlerResponse {mErrorMsg = errorMsg, mErrorType = errorType}

getEndpoint :: IO (Either Text (Text, Int))
getEndpoint = do
  ep <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
  case ep of
    Nothing -> pure $ Left "AWS_LAMBDA_RUNTIME_API not found in ENV"
    Just ep' -> pure $ Right (getHostAndPort (Text.pack ep'))
  where
    getHostAndPort endpoint =
      let getPort rawPort = fromMaybe 80 (readMaybe (Text.unpack rawPort))
       in case Text.splitOn ":" endpoint of
            [] -> ("", 80)
            [host] -> (host, 80)
            [host, portRaw] -> (host, getPort portRaw)
            host:portRaw:_ -> (host, getPort portRaw)

runHandler :: (HandlerRequest -> IO HandlerResponse) -> IO ()
runHandler handler = do
  hSetBuffering stdout LineBuffering
  print "Initialising the Haskell Lambda Runtime."
  ep <- getEndpoint
  either (error . Text.unpack) (loop handler) ep

loop :: (HandlerRequest -> IO HandlerResponse) -> (Text, Int) -> IO ()
loop handler endpoint = do
  print "Getting next invocation..."
  NextInvocation.Response rsp <- NextInvocation.getWithRetries 3 endpoint
  either handleError handleSuccess rsp
  where
    handleSuccess req = do
      print "Invoking user handler."
      res <- handler req
      print "Invoking user handler completed."
      InvocationCallback.run endpoint (requestId req) res
      loop handler endpoint
    handleError (NextInvocation.ErrorCode code) =
      case code of
        -1 -> print "Failed to send HTTP request to retrieve next task."
        _ -> do
          print $
            "HTTP request was not successful. HTTP response code: " ++
            show code ++ ". Retrying.."
          loop handler endpoint
