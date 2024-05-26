module DB (save) where

import Data.Aeson
import Model

save :: Model -> IO ()
save = encodeFile "test.txt"