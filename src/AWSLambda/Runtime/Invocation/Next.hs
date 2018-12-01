module AWSLambda.Runtime.Invocation.Next
  ( get
  , Response(..)
  , ErrorCode(..)
  ) where

import Data.Maybe (fromJust)
import Protolude hiding (get)

import Data.Default.Class (def)
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

get :: (Text, Int) -> IO Response
get (host, port) = do
  let url = Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: "next"
  Req.runReq def $ do
    rsp <- Req.req Req.GET url Req.NoReqBody Req.bsResponse (Req.port port)
    let code = Req.responseStatusCode rsp
    if not (code >= 200 && code <= 299)
      then do
        liftIO $
          print
            ("Failed to get next invocation. Http Response code: " <> show code :: Text)
        return $ Response (Left (ErrorCode code))
      else do
        let body = Req.responseBody rsp
        let mReqId = Req.responseHeader rsp "lambda-runtime-aws-request-id"
        case mReqId of
          Nothing -> do
            liftIO $
              print
                "Failed to find header lambda-runtime-aws-request-id in response"
            return $ Response (Left (ErrorCode (-1)))
          Just reqId ->
            return $
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
  where
    getHeader r header = TextEncoding.decodeUtf8 <$> Req.responseHeader r header
    getDeadline r = do
      millis <-
        (Text.unpack <$> getHeader r "lambda-runtime-deadline-ms" >>= readMaybe) :: Maybe Int
      Just $ posixSecondsToUTCTime (fromIntegral millis / 1000)
