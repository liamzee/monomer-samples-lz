{-

A version without comments:


{-# LANGUAGE OverloadedStrings #-}

import Monomer


data Model = EmptyModel deriving Eq

data Events = EmptyEvent


main = startApp EmptyModel eventHandler uiBuilder []

eventHandler _ _ EmptyModel EmptyEvent = []

uiBuilder _ EmptyModel = label ""



-}




{-# LANGUAGE OverloadedStrings #-} -- Used to allow the use of the text
                                   -- type for string literals.

{-

The minimal application that gets startApp to run a monomer application. Or, in other words,
this application does nothing, but it shows the minimal configuration to open a monomer window.

The data NullModel and data Events types can have no entries (i.e, drop the NulLEvent and NullModel),
but it breaks type inference, and in this sample, we want a version without type signatures used in idiomatic Haskell.

-}

import Monomer -- The import line, which imports everything exposed in the monomer package.

{-

Idiomatically, we declare our types before non-type bindings, but Haskell
is fully lazy and does not require declare before use.

-}

-- | Monomer requires a model data type that supports the Eq typeclass.
-- Here, we're using a minimally populated datatype to help
-- the type inference engine along.

data Model = EmptyModel deriving Eq

-- | Monomer also requires an event type, with the same caveats as for the
-- NullModel type.

data Events = EmptyEvent

-- | Haskell runs by looking for main in the main function,
-- then executing its IO value.
-- startApp, given an initial Model value, an eventHandler function, a
-- uiBuilder function, and a list/array (it's technically a linked list)
-- of configuration options, opens the window.

main = startApp EmptyModel eventHandler uiBuilder []

-- | The event handler function. The model and event arguments are pattern matched
-- (this function will throw an exception if given a generic model or event
-- value, since it'll run out of pattern matches) to help with type inference.
-- The event handler function, obviously, does nothing here, and returns
-- an empty list.

eventHandler _ _ EmptyModel EmptyEvent = [] -- Here, _ represents a wildcard
                                            -- pattern, which indicates
                                            -- that the argument is not used
                                            -- and cannot be used.

-- | The UI builder function. As above, the NullModel is added to aid in type inference,
-- and the function is partial (will crash if given a generic model value). It also does
-- nothing.

uiBuilder _ EmptyModel = label ""