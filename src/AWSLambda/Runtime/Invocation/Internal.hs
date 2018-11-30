module AWSLambda.Runtime.Invocation.Internal
  ( ErrorCode(..)
  ) where

import Protolude

newtype ErrorCode =
  ErrorCode Int
