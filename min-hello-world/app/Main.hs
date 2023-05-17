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