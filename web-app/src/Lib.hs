{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( handleRequest
    , muddyStart
    ) where

import Model
import Web.Scotty
-- import Network.HTTP.Types

muddyStart :: Model
muddyStart = Mo
  [0,1,2,3,4,5,6,7]
  [("1",[[0,4],[2,6],[3,7],[1,5]])
  ,("2",[[0,2],[4,6],[5,7],[1,3]])
  ,("3",[[0,1],[4,5],[6,7],[2,3]])]
  [(0,[]) ,(1,[3])  ,(2,[2])  ,(3,[2,3])
  ,(4,[1]),(5,[1,3]),(6,[1,2]),(7,[1,2,3])]

handleRequest :: IO ()
handleRequest = 
    scotty 3000 $ do
        get "/" $ do                         -- handle GET request on "/" URL
            text "This was a GET request!"     -- send 'text/plain' response
        delete "/" $ do
            html "This was a DELETE request!"  -- send 'text/html' response
        post "/" $ do
            text "This was a POST request!"
        put "/" $ do
            text "This was a PUT request!"
        
        -- get model (json)
        get "/model" $ do
            json muddyStart -- Call Model constructor and encode the result as JSON

        -- post model (json)
        post "/model" $ do
            model <- jsonData :: ActionM Model -- Decode body of the POST request as an Model object
            json model     

        -- handler for when there is no matched route
        -- (this should be the last handler because it matches all routes)
        notFound $ do
            text "there is no such route."
