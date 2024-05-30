-- \section{Wrapping it up in an exectuable}\label{sec:Main}

-- We will now use the library form Section \ref{sec:Request} in a program.

-- \begin{code}
module Main where

import Request

main :: IO ()
main = handleRequest

-- \end{code}

-- We can run this program with the commands:

-- \begin{verbatim}
-- stack build
-- stack exec myprogram
-- \end{verbatim}

-- The output of the program is something like this:

-- \begin{verbatim}
-- Setting phasers to stun... (port 3000) (ctrl-c to quit)

-- \end{verbatim}
