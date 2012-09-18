-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Unsafe
    ( unsafeReturn  -- Print out string as side effect of return, show value
    , unsafeRet     -- Print out a string as side effect of return
    , unsafeOut     -- Print out from anywhere
    , IOE.trace     -- Re-export trace from IO extenstions
    , traceShow     -- Trace a value with a header string
    , traceShow2    -- Trace execution
    ) where

-- Haskell imports
import qualified IOExts as IOE
import qualified IO as IO

-- Print out the string as a side effect of returning the value.
-- Also show the value
unsafeReturn :: (Show a) =>
    String -> -- String to print
    a ->      -- Value to return
    a         -- The returned value
unsafeReturn s a =
  IOE.unsafePerformIO ( do { putStrLn (s ++ " = " ++ show a)
                           ; IO.hFlush IO.stdout
                           ; return a
                           }
                      )

-- Print out the string as a side effect of returning the value.
unsafeRet :: String -> a -> a
unsafeRet s a =
  IOE.unsafePerformIO ( do { putStrLn (s)
                           ; IO.hFlush IO.stdout
                           ; return a
                           }
                      )

-- Let the programmer put a print out in the middle of evaluation
-- of an expression
unsafeOut :: IO () -> a -> a
unsafeOut out a =
  IOE.unsafePerformIO
    ( do { out
         ; return a
         }
    )

traceShow :: Show a => String -> a -> a
traceShow msg x = IOE.trace (msg ++ show x ++ "\n") x

traceShow2  :: String -> a -> a
traceShow2  msg x = IOE.trace (msg ++ "\n") x
