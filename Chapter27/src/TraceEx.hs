module TraceEx where

import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes =
    inc (trace "I got eval'd" (1+1))
    + twice (trace "I got eval'd" (1+1))

howManyTimes' =
    let onePlusOne = trace "I got eval'd" (1+1)
 in inc onePlusOne + twice onePlusOne

{- 
    Prelude> howManyTimes
    I got eval'd
    I got eval'd
    7

    Prelude> howManyTimes'
    I got eval'd
    7
    -}
