module AWSLambda.Runtime.Handler.Response
  ( HandlerResponse(..)
  , isSuccess
  , getPayload
  ) where

import Protolude

import Data.Aeson
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding
import GHC.Exts (fromList)

data HandlerResponse
  = SuccessHandlerResponse { mPayload :: Text
                           , mContentType :: Maybe Text }
  | FailureHandlerResponse { mErrorMsg :: Text
                           , mErrorType :: Text }

isSuccess :: HandlerResponse -> Bool
isSuccess SuccessHandlerResponse {} = True
isSuccess FailureHandlerResponse {} = False

getPayload :: HandlerResponse -> Text
getPayload rsp@SuccessHandlerResponse {} = mPayload rsp
getPayload rsp@FailureHandlerResponse {} =
  LazyText.toStrict $ LazyTextEncoding.decodeUtf8 $ encode v
  where
    v :: Value
    v =
      Object $
      fromList
        [ ("errorMessage", String (mErrorMsg rsp))
        , ("errorType", String (mErrorType rsp))
        , ("stackTrace", Array $ fromList [])
        ]
