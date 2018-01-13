module Main where

import Lens.Micro ((&), (.~))
import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , eaTopL
  , eaBottomL
  , offerJoinL
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , hBox
  , vBox
  , hLimit
  , vLimit
  , joinBorders
  , modifyBorderDynamics
  , withBorderStyle
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

doubleHorizontal :: BS.BorderStyle
doubleHorizontal = BS.BorderStyle
    { BS.bsCornerTL = '╒'
    , BS.bsCornerTR = '╕'
    , BS.bsCornerBR = '╛'
    , BS.bsCornerBL = '╘'
    , BS.bsIntersectL = '╞'
    , BS.bsIntersectR = '╡'
    , BS.bsIntersectT = '╤'
    , BS.bsIntersectB = '╧'
    , BS.bsIntersectFull = '╪'
    , BS.bsHorizontal = '═'
    , BS.bsVertical = '│'
    }

box1 :: Widget ()
box1 = B.borderWithLabel
    (modifyBorderDynamics (& eaBottomL.offerJoinL .~ False) (B.vBorder <+> str "label" <+> B.vBorder))
    $ B.border
    $ hBox [ C.vCenter $ hLimit 5 B.hBorder
           , vBox [ C.hCenter $ vLimit 2 B.vBorder
                  , hBox [ modifyBorderDynamics (\b -> b & eaTopL.offerJoinL .~ False
                                                         & eaBottomL.offerJoinL .~ False
                                                )
                                                B.vBorder
                         , vBox [B.hBorder, C.center (str "content A"), B.hBorder]
                         , modifyBorderDynamics (\b -> b & eaTopL.offerJoinL .~ False
                                                         & eaBottomL.offerJoinL .~ False
                                                )
                                                B.vBorder
                         ]
                  , C.hCenter $ vLimit 2 B.vBorder
                  ]
           , C.vCenter $ hLimit 5 B.hBorder
           ]

box2 :: Widget ()
box2 = withBorderStyle doubleHorizontal . B.border $ vBox
    [ hBox
        [ C.vCenter (str "content B")
        , B.vBorder
        , C.vCenter (str "content C")
        , B.vBorder
        , C.center (str "content D")
        ]
    , B.hBorder
    , withBorderStyle BS.unicode $ hBox
        [ C.center (str "content E")
        , B.vBorder
        , C.hCenter (str ("content F")) <=> B.hBorder <=> C.center (str ("content G"))
        , B.vBorder
        , C.center (str "content H")
        ]
    ]

ui :: Widget ()
ui = joinBorders (box1 <=> box2)

main :: IO ()
main = M.simpleMain ui
