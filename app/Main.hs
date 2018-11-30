module Main where

import AWSLambda.Runtime (Request, Response, mkSuccessResponse, runHandler)
import Protolude

main :: IO ()
main = runHandler myHandler

myHandler :: Request -> IO Response
myHandler req = do
  print "Log line"
  return $ mkSuccessResponse "{ \"success\" : \"true\" }" "application/json"
