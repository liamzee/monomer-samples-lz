{- Uncommented version:

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Monomer


data Model = EmptyModel deriving Eq

data Events 


main :: IO ()
main = startApp EmptyModel eventHandler uiBuilder []

eventHandler :: AppEventHandler Model Events
eventHandler _ _ _ _ = []

uiBuilder :: AppUIBuilder Model Events
uiBuilder _ _ = label ""

-}



{-# LANGUAGE OverloadedStrings #-} -- Used to allow the use of the text
                                   -- type for string literals.

{-

The minimal application that gets startApp to run a monomer application. Or, in other words,
this application does nothing, but it shows the minimal configuration to open a monomer window.

The data NullModel and data Events types can have no entries (i.e, drop the NulLEvent and NullModel),
but it breaks type inference, and in this sample, we want a version without idiomatic type signatures.

-}

-- Start of module.

module Main where

import Monomer -- The import line, which gives you everything in the monomer package.

{-

Idiomatically, we declare our types before non-type bindings, but Haskell
is fully lazy and does not require declare before use.

-}

-- | Monomer requires a model data type that supports the Eq typeclass.
-- Here, we're using a minimally populated datatype to satisfy
-- the requirement of startApp for an initial model.

data Model = EmptyModel deriving Eq

-- | Monomer also requires an event type. Here, we're using a void type,
-- that is, a type with no values.

data Events 

-- | Haskell runs by looking for main in the main function,
-- then executing its IO value.
-- startApp, given an initial Model value, an eventHandler function, a
-- uiBuilder function, and a list/array (it's technically a linked list)
-- of configuration options, opens the window.

main :: IO ()
main = startApp EmptyModel eventHandler uiBuilder []

-- | The event handler function. Since we're not using any of the arguments,
-- we're wildcarding everything with _, which means an argument that is required,
-- but not used.
--
-- The AppEventHandler type is a type synonym for
--
-- WidgetEnv s e -> WidgetNode s e -> s -> e -> [AppEventResponse s e]
--
-- where s is a model type (here, Model), e is an event type (Events),
-- WidgetEnv denotes a widget environment, WidgetNode denotes a root node,
-- s denotes the model, and e denotes the events.
--
-- In otherwords, it's a function that takes a WidgetEnv, a WidgetNode,
-- a model value, and an event value, and returns a list of
-- AppEventResponses.

eventHandler :: AppEventHandler Model Events
eventHandler _ _ _ _ = []

-- | The UI builder function that does nothing. As before, we wildcard all arguments.
-- 
-- As for the AppUIBuilder type, it is a type synonym for UIBuilder, which is a type
-- synonym itself for
--
-- WidgetEnv s e -> s -> WidgetNode s e,
--
-- i.e, it's a function that takes a WidgetEnv (widget environment), a Model value,
-- and returns a WidgetNode that the framework uses
-- to build the GUI.

uiBuilder :: AppUIBuilder Model Events
uiBuilder _ _ = label ""