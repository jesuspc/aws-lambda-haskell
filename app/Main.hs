module Main where

import AWSLambda.Runtime
  ( HandlerRequest
  , HandlerResponse
  , mkSuccessResponse
  , runHandler
  )
import Protolude

main :: IO ()
main = runHandler myHandler

myHandler :: HandlerRequest -> IO HandlerResponse
myHandler req = do
  print "Log line"
  return $ mkSuccessResponse "{ \"success\" : \"true\" }" "application/json"
