{-

Simplest possible hello world.

Version without comments:

{-# LANGUAGE OverloadedStrings #-}

import Monomer

main = startApp () eventHandler uiBuilder []

eventHandler _ _ () () = []

uiBuilder _ () =
      image_
        "https://upload.wikimedia.org/wikipedia/\
        \commons/thumb/7/74/HelloWorld_in_black_and\
        \_white.svg/2880px-HelloWorld_in_black_and_white.svg.png"
        [fitFill]

-}

{-# LANGUAGE OverloadedStrings #-} -- Like many other "serious"
                                   -- Haskell libraries and programs,
                                   -- monomer uses the Text datatype
                                   -- in lieu of String, necessitating
                                   -- the OverloadedStrings language pragma.
                                   -- This allows String literals to be interpreted
                                   -- as different types, eschewing the low-efficiency
                                   -- String, or linked-list-of-chars type, for Text.

{- Here we import the monomer module, implicitly importing
-- everything useful and not hidden in the monomer library.

import Monomer

-- | startApp is a function that takes a model value, an eventHandler function,
-- a UI builder function, and a list of configuration options,
-- then calls the monomer library to open a window for us.
--
-- Here, we are using the value (), unit, which is equivalent to void
-- in other languages, to trick monomer into starting without a proper
-- model value.
--
-- As for main, every compiled Haskell program requires a main value,
-- which is then interpreted as a set of instructions for the runtime
-- to run.

main = startApp () eventHandler uiBuilder []

-- | The event handler outputs a "list of requested actions", as per the
-- documentation, which is then evaluated by the monomer library.
--
-- For arguments, we're using the wildcard character to dummy the first two
-- arguments, since we're not using anything and just outputting an empty list.
-- The last two arguments are satisfied by (), allowing the type inference
-- engine to give the eventHandler a proper type.

eventHandler _ _ () () = []

-- | The UI builder is what actually gives us our image. Here, we dummy the first
-- argument with the wildcard symbol, and the second argument with unit to
-- help the type inference engine along.
--
-- As for its actual output, we are returning a widget
-- that displays an image, since monomer cannot output text
-- without having a font set up in its configuration settings, and currently
-- we require that the font be included in an asset list
-- in the cabal file.

uiBuilder _ () =
      image_
        "https://upload.wikimedia.org/wikipedia/\
        \commons/thumb/7/74/HelloWorld_in_black_and\
        \_white.svg/2880px-HelloWorld_in_black_and_white.svg.png"
        [fitFill]