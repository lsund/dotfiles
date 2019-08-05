module Config where

import XMonad.Layout.Tabbed

-- Looks
backgroundImage = "/home/lsund/.xmonad/img/spy.jpg"

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

-- gnomeTerm = "/usr/bin/gnome-terminal"
urxvtTerm = "/usr/bin/urxvt"

logpath = "/home/lsund/Data/log/"

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

spHeight = 0.5 :: Rational

spWidth = 1 :: Rational

spTopDist = 1 - spHeight

spLeftDist = 1 - spWidth
