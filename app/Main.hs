module Main (main) where

import Monomer

import Model
import UI

main :: IO ()
main = do
    let config =
            [ appWindowState $ MainWindowNormal (864, 640)
            , appWindowResizable False
            , appWindowTitle "Monomer chess"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
    startApp initModel handleEvent buildUI config
