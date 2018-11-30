module AWSLambda.Runtime.Internal where

import Data.Time.Clock (UTCTime)
import Protolude

data HandlerRequest = HandlerRequest
  { payload :: Text
  , requestId :: Text
  , xrayTraceId :: Text
  , clientContext :: Text
  , cognitoIdentity :: Text
  , functionArn :: Text
  , deadline :: UTCTime
  }

data HandlerResponse = HandlerResponse
  { mPayload :: Text
  , mContentType :: Text
  , mSuccess :: Bool
  }
