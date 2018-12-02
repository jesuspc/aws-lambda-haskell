module AWSLambda.Runtime.Invocation.Client
  ( Response(..)
  , ErrorCode(..)
  , mkErrorResponse
  , mkSuccessResponse
  , isSuccessCode
  ) where

import Protolude

newtype ErrorCode =
  ErrorCode Int
  deriving (Show)

data Response a
  = SuccessResponse a
  | ErrorResponse ErrorCode

mkErrorResponse :: Int -> Response a
mkErrorResponse code = ErrorResponse (ErrorCode code)

mkSuccessResponse :: a -> Response a
mkSuccessResponse = SuccessResponse

isSuccessCode :: Int -> Bool
isSuccessCode code = code >= 200 && code <= 299
