module Main (main) where

import Monomer
import System.Random

import Model
import UI

main :: IO ()
main = do
    g <- newStdGen
    let model = initModel g
        config =
            [ appWindowState $ MainWindowNormal (864, 640)
            , appWindowResizable False
            , appWindowTitle "Monomer chess"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
    startApp model handleEvent buildUI config
