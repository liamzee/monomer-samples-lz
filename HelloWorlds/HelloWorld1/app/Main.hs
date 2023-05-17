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
uiBuilder _ _ =
    image_
        "https://upload.wikimedia.org/\
        \wikipedia/commons/thumb/2/28/\
        \HelloWorld.svg/512px-HelloWorld\
        \.svg.png?20100207111424"
        [fitFill]



-}


{-# LANGUAGE OverloadedStrings #-}
module Main () where -- Here, for the first time, we add a minimum
                     -- export list to our module declaration.
                     -- This list is empty, meaning that
                     -- no names from this module are exported
                     -- to other modules, but we could very well
                     -- include various names for use elsewhere.
                     -- Please note, () allows the export of
                     -- "orphan instances", which is a bit
                     -- complicated to explain but not relevant
                     -- at this time.

import Monomer

{- If anything seems confusing here, consult the minimum apps examples
to get clarification. Otherwise, there's nothing to note, other than
the introduction of an empty export list. -}

data Model = EmptyModel deriving Eq

data Events

{-# Nothing really unusual going on here, it's the same as before.

We use a Void type, one that has no values, to satisfy the type
requirement of events.

We use deriving because the model type the framework is asking for
requires an Eq, or equals, instance.

-}

main :: IO ()
main = startApp EmptyModel eventHandler uiBuilder []

eventHandler :: AppEventHandler Model Events
eventHandler _ _ _ _ = []

{- As before, we use startApp on a model value, then feed it
an event handler function, a ui builder, and a list of configuration values.

The event handler is unchanged from the minimum idiomatic example.
-}

-- | Here, we have something interesting. First, we are expressing our hello world
-- with an image. This is because we'll have to set configuration options for fonts
-- before we can display any text.
--
-- Second, we are now using a widget created by the function image_, which takes
-- a text field, as well as a list of configuration arguments either as values
-- or generated by a function.
--
-- You can see the full list of configuration options here, for the 1.5.1.0 version.
-- https://hackage.haskell.org/package/monomer-1.5.1.0/docs/Monomer-Widgets-Singles-Image.html#t:ImageCfg

uiBuilder :: AppUIBuilder Model Events
uiBuilder _ _ =
    image_
        "https://upload.wikimedia.org/\   -- Note here that we are using a '\' character
        \wikipedia/commons/thumb/2/28/\   -- to split the string literal, then start the
        \HelloWorld.svg/512px-HelloWorld\ -- text again with the same escape character.
        \.svg.png?20100207111424"         -- If we need to show an escape character,
                                          -- we'll have to start the line first with \, then
                                          -- append the escape character.

        [fitFill]  -- And here we use fitFill to tell monomer that the image
                   -- should be stretched to fit the widget. Otherwise, it'd be aligned
                   -- up and left, at its normal image size.