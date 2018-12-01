module AWSLambda.Runtime.Handler.Request
  ( HandlerRequest(..)
  ) where

import Protolude

import Data.Time.Clock (UTCTime)

data HandlerRequest = HandlerRequest
  { payload :: Text
  , requestId :: Text
  , xrayTraceId :: Maybe Text
  , clientContext :: Maybe Text
  , cognitoIdentity :: Maybe Text
  , functionArn :: Maybe Text
  , deadline :: Maybe UTCTime
  }
