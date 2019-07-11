
module Boxes where

import Config

-- Dzen box pretty config
data BoxPP = BoxPP
    { bgColorBPP   :: String
    , fgColorBPP   :: String
    , boxColorBPP  :: String
    , leftIconBPP  :: String
    , rightIconBPP :: String
    , boxHeightBPP :: Int
    }

-- Uses dzen format to draw a "box" arround a given text
dzenBoxStyle :: BoxPP -> String -> String
dzenBoxStyle bpp t =
    "^fg(" ++ boxColorBPP bpp ++
    ")^i(" ++ leftIconBPP bpp  ++
    ")^ib(1)^r(1920x" ++ show (boxHeightBPP bpp) ++
    ")^p(-1920)^fg(" ++ fgColorBPP bpp ++
    ")" ++ t ++
    "^fg(" ++ boxColorBPP bpp ++
    ")^i(" ++ rightIconBPP bpp ++
    ")^fg(" ++ bgColorBPP bpp ++
    ")^r(1920x" ++ show (boxHeightBPP bpp) ++
    ")^p(-1920)^fg()^ib(0)"

-- Dzen logger box pretty printing themes
gray2BoxPP :: BoxPP
gray2BoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorGray
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

blue2BoxPP :: BoxPP
blue2BoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlue
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
    { bgColorBPP   = colorGrayAlt
    , fgColorBPP   = colorWhite
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

red2BBoxPP :: BoxPP
red2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorRed
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

blue2BBoxPP :: BoxPP --current workspace
blue2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorBlue
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

green2BBoxPP :: BoxPP
green2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorGreen
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

gray2BBoxPP :: BoxPP
gray2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorGray
    , leftIconBPP  = boxLeftIcon2
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }


