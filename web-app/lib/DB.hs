-- \section{Database}\label{sec:Database}
-- This section describes the database implementation

-- \begin{code}
module DB (save) where

import Data.Aeson
import Model

save :: Model -> IO ()
save = encodeFile "test.txt"

get :: IO Model
get = decodeFile ""

-- \end{code}