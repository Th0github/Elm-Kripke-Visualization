{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( handleRequest,
    muddyStart,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DB
import Model
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCors, simpleCorsResourcePolicy, simpleHeaders, simpleMethods)
import Web.Scotty
  ( ActionM,
    delete,
    get,
    html,
    json,
    jsonData,
    middleware,
    notFound,
    post,
    put,
    scotty,
    text,
  )

-- import Network.HTTP.Types

muddyStart :: Model
muddyStart =
  Mo
    [0, 1, 2, 3, 4, 5, 6, 7]
    [ ("1", [[0, 4], [2, 6], [3, 7], [1, 5]]),
      ("2", [[0, 2], [4, 6], [5, 7], [1, 3]]),
      ("3", [[0, 1], [4, 5], [6, 7], [2, 3]])
    ]
    [ (0, []),
      (1, [3]),
      (2, [2]),
      (3, [2, 3]),
      (4, [1]),
      (5, [1, 3]),
      (6, [1, 2]),
      (7, [1, 2, 3])
    ]

handleRequest :: IO ()
handleRequest = scotty 3000 $ do
  middleware corsMiddleware
  get "/" $ do
    -- handle GET request on "/" URL
    text "This was a GET request!" -- send 'text/plain' response
  delete "/" $ do
    html "This was a DELETE request!" -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"

  -- get model (json)
  get "/model" $ do
    json muddyStart -- Call Model constructor and encode the result as JSON
  post "/model" $ do
    model <- jsonData :: ActionM Model -- Decode body of the POST request as an Model object
    liftIO $ save model
    json model
  notFound $ do
    text "there is no such route."
  where
    corsMiddleware = cors (const $ Just resourcePolicy)
      where
        resourcePolicy =
          simpleCorsResourcePolicy
            { corsMethods = "DELETE" : "PUT" : simpleMethods, -- simpleMethods are GET,HEAD,POST
              corsRequestHeaders = "Content-Type" : simpleHeaders
            }

-- handler for when there is no matched route
-- (this should be the last handler because it matches all routes)
