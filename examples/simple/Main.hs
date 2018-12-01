module Main where

import AWSLambda.Runtime
  ( HandlerRequest
  , HandlerResponse
  , mkSuccessResponse
  , runHandler
  )
import Protolude

-- runHandler starts the haskell runtime that will fetch any invocation and forward it to the user defined handler
main :: IO ()
main = runHandler myHandler

-- See the types for HandlerRequest and HandlerResponse
myHandler :: HandlerRequest -> IO HandlerResponse
myHandler req = do
  print "Log line"
  return $ mkSuccessResponse "{ \"success\" : \"true\" }" "application/json"
