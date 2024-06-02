module DB (saveModel, getModel) where

import Data.Aeson
import Model

saveModel :: Model -> IO ()
saveModel = encodeFile "test.txt"

getModel :: IO (Maybe Model)
getModel = decodeFileStrict "test.txt"