module Debug
    ( trace
    , _trace
    , traceList
    ,_traceList
    ) where

import Data.List
import qualified Debug.Trace as Trace

trace :: Show a => String -> a -> a
trace message x = Trace.trace (message ++ " = " ++ show x) x

_trace :: String -> a -> a
_trace _ = id

traceList :: Show a => String -> [a] -> [a]
traceList message xs = Trace.trace (message ++ " = [\n\t" ++ intercalate "\n\t" (map show xs) ++ "\n]") xs

_traceList :: Show a => String -> [a] -> [a]
_traceList _ = id
