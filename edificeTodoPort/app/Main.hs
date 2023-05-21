{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Monomer
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import Data.FileEmbed (makeRelativeToProject, embedFile)
import qualified Data.ByteString as BS

data GState
   = MkGState
   { myLog :: !(Seq Text)
   , counter :: !Int
   , fieldText :: !Text
   } deriving Eq

data Events = Increment | UpdateText !Text

main :: IO ()
main = startApp (MkGState Seq.empty 0 "") eventHandler uiBuilder
    [ appFontDefMem "Regular" robotoReg
    , appTheme darkTheme
    , appWindowTitle "Primitive ToDo List"]

robotoReg :: BS.ByteString
robotoReg = $(makeRelativeToProject "app/RobotoRegular.ttf" >>= embedFile)

eventHandler :: AppEventHandler GState Events
eventHandler _ _ state event = case event of
    Increment -> [Model state{ myLog = myLog state Seq.|> fieldText state, counter = counter state + 1, fieldText = ""}]
    (UpdateText newText) -> [Model state { fieldText = newText } ]--, --SetFocusOnKey "TextField" ]

uiBuilder :: AppUIBuilder GState Events
uiBuilder _ state = box_ [alignBottom] . flip styleBasic [padding 24] $ vstack
    [ vscroll . vstack $ flip styleBasic [textSize 24, padding 6] . label <$> myLog state
    , box_ [alignBottom] $ hstack
        [ textFieldV (fieldText state) UpdateText 
        , mainButton ("To Do #" <> (pack . show $ counter state)) Increment
        ]
    ]