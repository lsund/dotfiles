module Config where

import XMonad.Layout.Tabbed

-- Looks
backgroundImage = "/home/lsund/Data/img/wallpaper.png"

cpuUsagePath = "/home/lsund/.xmonad/apps/haskell-cpu-usage.out"

dzenFont = "xft:Hack:regular:size=14"

colorBlack = "#020202"

colorBlackAlt = "#1c1c1c"

colorGray = "#444444"

colorGrayAlt = "#101010"

colorGrayAlt2 = "#404040"

colorGrayAlt3 = "#252525"

colorWhite = "#a9a6af"

colorWhiteAlt = "#9d9d9d"

colorWhiteAlt2 = "#b5b3b3"

colorWhiteAlt3 = "#707070"

colorMagenta = "#8e82a2"

colorBlue = "#44aacc"

colorBlueAlt = "#3955c4"

colorRed = "#f7a16e"

colorRedAlt = "#e0105f"

colorGreen = "#66ff66"

colorGreenAlt = "#558965"

boxLeftIcon2 = "/home/lsund/.icons/xbm_icons/subtle/boxleft2-big.xbm"

boxRightIcon = "/home/lsund/.icons/xbm_icons/subtle/boxright.xbm"

panelHeight = 24 :: Int

boxHeight = 22 :: Int

topPanelSepPos = 950 :: Int

botPanelSepPos = 900 :: Int

maxTitleLen = 200 :: Int

gnomeTerm = "/usr/bin/alacritty"

urxvtTerm = "/usr/bin/urxvt"

logpath = "/home/lsund/Data/log/"

syncPath = logpath ++ "pacman/pacsynccount.txt"

scriptpath = "/home/lsund/Documents/dotfiles/shell/"

-- Title theme
myTitleTheme :: Theme
myTitleTheme =
  def
    { fontName = dzenFont
    , inactiveBorderColor = colorGrayAlt2
    , inactiveColor = colorGrayAlt3
    , inactiveTextColor = colorWhiteAlt3
    , activeBorderColor = colorGrayAlt2
    , activeColor = colorGrayAlt2
    , activeTextColor = colorWhiteAlt2
    , decoHeight = 24
    }

-- ScratchPad
spHeight = 0.4 :: Rational
spWidth = 1 :: Rational
spTopDist = 1 - spHeight
spLeftDist = 1 - spWidth

--------------------------------------------------------------------------------
-- DZEN

data DzenFlags =
  DzenFlags
    { xPosDF :: Int
    , yPosDF :: Int
    , widthDF :: Int
    , heightDF :: Int
    , alignementDF :: String
    , fgColorDF :: String
    , bgColorDF :: String
    , fontDF :: String
    , eventDF :: String
    , extrasDF :: String
    }

data Res =
  Res
    { xRes :: Int
    , yRes :: Int
    }

-- data Layout = TL | TR | BL | BR

-- dzenFlags :: Layout -> Res -> DzenFlags

dzenTopLeftFlags :: Res -> DzenFlags
dzenTopLeftFlags _ =
  DzenFlags
    { xPosDF = 0
    , yPosDF = 0
    , widthDF = topPanelSepPos
    , heightDF = panelHeight
    , alignementDF = "l"
    , fgColorDF = colorWhiteAlt
    , bgColorDF = colorBlack
    , fontDF = dzenFont
    , eventDF = "onstart=lower"
    , extrasDF = "-p"
    }

-- Dzen top right bar flags
dzenTopRightFlags :: Res -> DzenFlags
dzenTopRightFlags r =
  DzenFlags
    { xPosDF = topPanelSepPos
    , yPosDF = 0
    , widthDF = xRes r - topPanelSepPos
    , heightDF = panelHeight
    , alignementDF = "r"
    , fgColorDF = colorWhiteAlt
    , bgColorDF = colorBlack
    , fontDF = dzenFont
    , eventDF = "onstart=lower"
    , extrasDF = "-p"
    }

-- Dzen bottom left bar flags
dzenBotLeftFlags :: Res -> DzenFlags
dzenBotLeftFlags r =
  DzenFlags
    { xPosDF = 0
    , yPosDF = yRes r - panelHeight
    , widthDF = botPanelSepPos
    , heightDF = panelHeight
    , alignementDF = "l"
    , fgColorDF = colorWhiteAlt
    , bgColorDF = colorBlack
    , fontDF = dzenFont
    , eventDF = "onstart=lower"
    , extrasDF = "-p"
    }

-- Dzen bottom right bar flags
dzenBotRightFlags :: Res -> DzenFlags
dzenBotRightFlags r =
  DzenFlags
    { xPosDF = botPanelSepPos
    , yPosDF = yRes r - panelHeight
    , widthDF = xRes r - botPanelSepPos
    , heightDF = panelHeight
    , alignementDF = "r"
    , fgColorDF = colorBlue
    , bgColorDF = colorBlack
    , fontDF = dzenFont
    , eventDF = "onstart=lower"
    , extrasDF = "-p"
    }
