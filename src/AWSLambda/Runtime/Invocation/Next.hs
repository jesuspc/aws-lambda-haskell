module AWSLambda.Runtime.Invocation.Next
  ( get
  , Response(..)
  , ErrorCode(..)
  ) where

import Protolude hiding (get)

import AWSLambda.Runtime.Invocation.Internal
import Data.Default.Class (def)
import qualified Network.HTTP.Req as Req

import AWSLambda.Runtime.Internal

newtype Response =
  Response (Either ErrorCode HandlerRequest)

get :: Text -> IO Response
get endpoint = do
  let url = endpoint <> "/2018-06-01/runtime/invocation/next"
  a <- Req.runReq def doReq
  return $ Response (Left (ErrorCode 400))
  where
    doReq = do
      r <-
        Req.req
          Req.GET
          (Req.http endpoint)
          Req.NoReqBody
          Req.jsonResponse
          mempty
      let code = Req.responseStatusCode r
      if not (code >= 200 && code <= 299)
        then do
          liftIO $
            print $
            "Failed to get next invocation. Http Response code: " <> show code
          undefined
        else undefined
