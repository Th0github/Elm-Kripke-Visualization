-- \section{The most basic library}\label{sec:Request}
-- This section describes request handling
-- \begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Request where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DB
import EvaluationRequest
import Form
import Model
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy, simpleHeaders, simpleMethods)
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
    request,
    scotty,
    text,
  )

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
    model <- liftIO getModel
    json model -- Call Model constructor and encode the result as JSON
  post "/model" $ do
    model <- jsonData :: ActionM Model -- Decode body of the POST request as an Model object
    liftIO $ saveModel model
    json model
  post "/evaluate" $ do
    r <- jsonData :: ActionM EvaluationRequest
    let trueWorlds = getFormFromRequest r `trueIn` getModelFromRequest r
    json trueWorlds
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

-- \end{code}

-- That's it, for now.