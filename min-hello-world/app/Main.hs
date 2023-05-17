{-# LANGUAGE OverloadedStrings #-}

import Monomer

main = startApp () eventHandler uiBuilder []

eventHandler _ _ () () = []

uiBuilder _ () =
      image_
        "https://upload.wikimedia.org/wikipedia/commons/thumb/2/28/HelloWorld.svg/512px-HelloWorld.svg.png"
        [fitFill]