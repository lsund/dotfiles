
-- Extensions
{-# LANGUAGE
    DeriveDataTypeable
   , FlexibleContexts
   , TypeSynonymInstances
   , NoMonomorphismRestriction
   , MultiParamTypeClasses #-}

-- Modules
import XMonad
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.MagicFocus
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Actions.ShowText
import XMonad.Actions.MouseResize
import Data.Monoid
import Data.List
import System.IO
import Control.Concurrent
import Graphics.X11.Xinerama
import Control.Applicative
import Control.Exception as E
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS

import KeyBindings (myKeys, myMouseBindings)
import Config
import Boxes

data Res = Res { xRes :: Int, yRes :: Int }

myRound :: Double -> Int -> Double
myRound f n = fromInteger ( round $ f * (10^n)) / (10.0^^n)

-- Main
main :: IO ()
main = do
    hostname' <- readFile "/etc/hostname"
    let hostname = init hostname'
    r <- getScreenRes ":0" 0
    topLeftBar  <- dzenSpawnPipe $ dzenTopLeftFlags r
    topRightBar <- dzenSpawnPipe $ dzenTopRightFlags r
    botLeftBar  <- dzenSpawnPipe $ dzenBotLeftFlags r
    botRightBar <- dzenSpawnPipe $ dzenBotRightFlags r
    xmonad $ def
        { terminal           = if hostname == "keysersoze" then urxvtTerm else gnomeTerm
        , modMask            = mod4Mask
        , focusFollowsMouse  = True
        , clickJustFocuses   = True
        , borderWidth        = 1
        , normalBorderColor  = colorBlackAlt
        , focusedBorderColor = colorWhite
        , workspaces         = myWorkspaces hostname
        , startupHook        = myStartupHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook hostname
        , manageHook         = myManageHook hostname
        , logHook            =
            myTopLeftLogHook topLeftBar hostname  <+>
            myTopRightLogHook topRightBar <+>
            myBotLeftLogHook botLeftBar hostname   <+>
            myBotRightLogHook botRightBar hostname <+>
            ewmhDesktopsLogHook           >>
            setWMName "LG3D"
        , keys               = myKeys hostname
        , mouseBindings      = myMouseBindings
        }


-- Title theme
myTitleTheme :: Theme
myTitleTheme = def
    { fontName            = dzenFont
    , inactiveBorderColor = colorGrayAlt2
    , inactiveColor       = colorGrayAlt3
    , inactiveTextColor   = colorWhiteAlt3
    , activeBorderColor   = colorGrayAlt2
    , activeColor         = colorGrayAlt2
    , activeTextColor     = colorWhiteAlt2
    , decoHeight          = 24
    }

-- Prompt theme
-- Flash text config
myTextConfig :: ShowTextConfig
myTextConfig = STC
    { st_font = dzenFont
    , st_bg   = colorBlack
    , st_fg   = colorWhite
    }

every n xs = case drop (n - 1) xs of
              (y : ys) -> y : every n ys
              [] -> []


myWorkspaces hostname =
    if hostname == "pedro" then
        let ws = withScreens 2 ["1", "2", "3", "4", "5"]
        in head ws : (every 2 (tail ws) ++ every 2 ws)
    else
        map show [1..9]


-- Layout names
myTileName = "Tiled"
myMosAName = "Mosaic"
myCst3Name = "Web"

myFTabName = "Full"
myFloaName = "Float"

----------------------------------------------------------------------------
-- Startup hook

myStartupHook =
    setDefaultCursor xC_left_ptr <+>
    spawn "feh --bg-max /home/lsund/.xmonad/img/spy.jpg &" <+>
    spawn "/usr/bin/killall haskell-cpu-usage.out" <+>
    liftIO (threadDelay 1000000) <+>
    spawn "xrandr --output DVI-I-1 --right-of HDMI-0 --output HDMI-0 --primary" <+>
    spawn "/home/lsund/.xmonad/apps/haskell-cpu-usage.out 5" <+>
        (startTimer 1 >>= XS.put . TID)

----------------------------------------------------------------------------
-- Handle event hook config

-- Wrapper for the Timer id, so it can be stored as custom mutable state
newtype TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
    initialValue = TID 0

-- Handle event hook
myHandleEventHook =
    fullscreenEventHook <+> docksEventHook <+>
    clockEventHook <+> handleTimerEvent <+>
    notFocusFloat where
        clockEventHook e = do                 --thanks to DarthFennec
            (TID t) <- XS.get                 --get the recent Timer id
            handleTimer t e $ do              --run the following if e matches the id
                startTimer 1 >>= XS.put . TID --restart the timer, store the new id
                ask >>= logHook . config      --get the loghook and run it
                return Nothing                --return required type
            return $ All True                 --return required type
        notFocusFloat = followOnlyIf (fmap not isFloat)
            where --Do not focusFollowMouse on Float layout
                isFloat =
                    isSuffixOf myFloaName <$> gets (description . W.layout . W.workspace . W.current . windowset)

----------------------------------------------------------------------------
-- Layout

-- Tabbed transformer (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform TABBED x k = k myFTabU $ const x

-- Floated transformer (W+ctl+f)
data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
    transform FLOATED x k = k myFloaU $ const x

-- Unique Layouts
myFTabU = smartBorders $ named ("Unique " ++ myFTabName) $ tabbedAlways shrinkText myTitleTheme
myFloaU = named ("Unique " ++ myFloaName) $ mouseResize $ noFrillsDeco shrinkText myTitleTheme simplestFloat


myLayoutHook hostname = avoidStruts $
  gaps [(U, panelHeight), (D, panelHeight)] $
  configurableNavigation noNavigateBorders $
  minimize $
  maximize $
  mkToggle (single TABBED) $
  mkToggle (single FLOATED) $
  mkToggle (single MIRROR) $
  mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) allLayouts
  where
    allLayouts =
      myToggleL myTile myTileName |||
      myToggleL myMosA myMosAName |||
      myToggleL myCst3 myCst3Name
        --layouts
    myTile = ResizableTall 1 0.03 0.5 []
    myMosA = MosaicAlt M.empty
    myCst3 =
      layoutN 1 (relBox 0 0 1 0.7) (Just $ relBox 0 0 1 1) myTabb $
        layoutAll (relBox 0 0.7 1 1) myTabb
    myTabb = tabbed shrinkText myTitleTheme
    myToggleL l n =
      smartBorders $
      toggleLayouts
        (named ("Switcher " ++ n) $ switcher l)
        (named ("Normal " ++ n) l)
      where
        switcher l =
          windowSwitcherDecoration shrinkText myTitleTheme $
          draggingVisualizer l


----------------------------------------------------------------------------
-- Manage Hook


myManageHook :: String -> ManageHook
myManageHook hostname =
    manageDocks <+>
    dynamicMasterHook <+>
    manageWindows hostname <+>
    manageScratchPad

manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.2   -- height
    w = 1     -- width
    t = 1 - h -- distance from top
    l = 1 - w -- distance from left edge

-- Manage Windows
manageWindows :: String -> ManageHook
manageWindows hostname = composeAll . concat $
    [ [ resource  =? r --> doIgnore                    | r <- myIgnores ]
    , [ className =? c --> doCenterFloat               | c <- myFloatCC ]
    , [ name      =? n --> doCenterFloat               | n <- myFloatCN ]
    , [ name      =? n --> doSideFloat NW              | n <- myFloatSN ]
    , [ className =? c --> doF W.focusDown             | c <- myFocusDC ]
    , [ currentWs =? (myWorkspaces hostname !! 1) --> keepMaster "Chromium"]
    , [ isFullscreen   --> doFullFloat ]
    ] where
        name      = stringProperty "WM_NAME"
        myIgnores = ["desktop", "desktop_window"]
        myWebS    = ["Firefox"]
        myChatS   = ["Pidgin", "Xchat"]
        myAlt3S   = ["Amule", "Transmission-gtk"]
        myFloatCC = ["MPlayer", "mplayer2"]
        myFloatCN = ["Choose a file", "Open Image", "Firefox Preferences"]
        myFloatSN = ["Event Tester"]
        myFocusDC = ["Event Tester", "Notify-osd"]
        keepMaster c = assertSlave <+> assertMaster where
            assertSlave = fmap (/= c) className --> doF W.swapDown
            assertMaster = className =? c --> doF W.swapMaster


----------------------------------------------------------------------------
-- Dzen bars

dzenTopLeftFlags :: Res -> DF
dzenTopLeftFlags _ = DF
    { xPosDF       = 0
    , yPosDF       = 0
    , widthDF      = topPanelSepPos
    , heightDF     = panelHeight
    , alignementDF = "l"
    , fgColorDF    = colorWhiteAlt
    , bgColorDF    = colorBlack
    , fontDF       = dzenFont
    , eventDF      = "onstart=lower"
    , extrasDF     = "-p"
    }

-- Top left bar logHook
myTopLeftLogHook h hostname = dynamicLogWithPP def
  { ppOutput          = hPutStrLn h
  , ppOrder           = \(ws:_:_:x) -> ws : x
  , ppSep             = " "
  , ppWsSep           = ""
  , ppCurrent         = dzenBoxStyle blue2BBoxPP
  , ppUrgent          = dzenBoxStyle red2BBoxPP
  , ppVisible         = dzenBoxStyle green2BBoxPP
  , ppHiddenNoWindows = dzenBoxStyle gray2BBoxPP
  , ppHidden          = dzenBoxStyle white2BBoxPP
  , ppExtras          = [ myFocusL ]
  }


-- Dzen top right bar flags
dzenTopRightFlags :: Res -> DF
dzenTopRightFlags r = DF
    { xPosDF       = topPanelSepPos
    , yPosDF       = 0
    , widthDF      = xRes r - topPanelSepPos
    , heightDF     = panelHeight
    , alignementDF = "r"
    , fgColorDF    = colorWhiteAlt
    , bgColorDF    = colorBlack
    , fontDF       = dzenFont
    , eventDF      = "onstart=lower"
    , extrasDF     = "-p"
    }

-- Top right bar logHook
myTopRightLogHook :: Handle -> X ()
myTopRightLogHook h = dynamicLogWithPP def
    { ppOutput  = hPutStrLn h
    , ppOrder   = \(_:_:_:x) -> x
    , ppSep     = " "
    , ppExtras  = [myUptimeL, myDateL]
    }

-- Dzen bottom left bar flags
dzenBotLeftFlags :: Res -> DF
dzenBotLeftFlags r = DF
    { xPosDF       = 0
    , yPosDF       = yRes r - panelHeight
    , widthDF      = botPanelSepPos
    , heightDF     = panelHeight
    , alignementDF = "l"
    , fgColorDF    = colorWhiteAlt
    , bgColorDF    = colorBlack
    , fontDF       = dzenFont
    , eventDF      = "onstart=lower"
    , extrasDF     = "-p"
    }

-- Bottom left bar logHook
myBotLeftLogHook :: Handle -> String -> X ()
myBotLeftLogHook h hostname =
  dynamicLogWithPP $
  def
  { ppOutput = hPutStrLn h
  , ppOrder = \(_:_:_:x) -> x
  , ppSep = " "
    , ppExtras = [myMemL, myRamL, myCpuL, myPacSyncL]
  }

-- Dzen bottom right bar flags
dzenBotRightFlags :: Res -> DF
dzenBotRightFlags r = DF
    { xPosDF       = botPanelSepPos
    , yPosDF       = yRes r - panelHeight
    , widthDF      = xRes r - botPanelSepPos
    , heightDF     = panelHeight
    , alignementDF = "r"
    , fgColorDF    = colorBlue
    , bgColorDF    = colorBlack
    , fontDF       = dzenFont
    , eventDF      = "onstart=lower"
    , extrasDF     = "-p"
    }

-- Bottom right bar logHook
myBotRightLogHook :: Handle -> String -> X ()
myBotRightLogHook h hostname =
  dynamicLogWithPP
    def
    { ppOutput = hPutStrLn h
    , ppOrder = \(_:_:_:x) -> x
    , ppSep = " "
    , ppExtras =
    if hostname == "pedro"
        then [mySoundL]
        else [myWifiL, myBrightL , mySoundL, myBatL hostname]
    }


----------------------------------------------------------------------------
-- Logger config


myBatL hostname =
    dzenBoxStyleL blue2BoxPP (labelL "bat ") ++!
    dzenBoxStyleL white2BBoxPP (batPercent hostname 30)


_memValues x = map (getValues x) $ take 4 [0..] where
    getValues x n = read (words (lines x !! n) !! 1) :: Int

_availMemKb :: String -> Int
_availMemKb x = read (words x !! 7) :: Int

_totMemKb :: String -> Int
_totMemKb x = read (words x !! 1) :: Int

myRamL =
    dzenBoxStyleL blue2BoxPP (labelL "mem") ++!
    dzenBoxStyleL white2BBoxPP (ramUsage [freeBMemUsage])
        where freeBMemUsage x =
                let free       = fromIntegral $ _availMemKb x
                    tot        = fromIntegral $ _totMemKb x
                    left       = tot - free
                    percLeft   = (left / tot) * 100
                in show (myRound percLeft 1) ++ "%"


myMemL =
    dzenBoxStyleL blue2BoxPP (labelL "disk") ++!
    dzenBoxStyleL white2BBoxPP memUsage

myCpuL =
    dzenBoxStyleL blue2BoxPP (labelL "cpu") ++!
    dzenBoxStyleL white2BBoxPP (cpuUsage "/tmp/haskell-cpu-usage.txt" 70 colorRed)

myWifiL =
    dzenBoxStyleL blue2BoxPP (labelL "wifi") ++!
    dzenBoxStyleL white2BBoxPP wifiStr

myBrightL =
    dzenBoxStyleL blue2BoxPP (labelL "bright") ++!
    dzenBoxStyleL white2BBoxPP (brightPerc 937)

mySoundL =
    dzenBoxStyleL blue2BoxPP (labelL "sound") ++!
    dzenBoxStyleL white2BBoxPP soundPerc

myPacSyncL =
    dzenBoxStyleL blue2BoxPP (labelL "sync ") ++!
    dzenBoxStyleL white2BBoxPP npacSync

-- TopRight Loggers
myDateL =
    dzenBoxStyleL white2BBoxPP (date "%A") ++!
    dzenBoxStyleL white2BBoxPP   (date $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg()") ++!
    dzenBoxStyleL white2BBoxPP   (date $ "%H^fg(" ++ colorGray ++ "):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()")

myUptimeL =
    dzenBoxStyleL blue2BoxPP   (labelL "upt") ++!
    dzenBoxStyleL white2BBoxPP uptime

myFocusL = dzenBoxStyleL white2BBoxPP (shortenL 100 logTitle)


----------------------------------------------------------------------------
-- Dzen utils

-- Dzen flags
data DF = DF
    { xPosDF       :: Int
    , yPosDF       :: Int
    , widthDF      :: Int
    , heightDF     :: Int
    , alignementDF :: String
    , fgColorDF    :: String
    , bgColorDF    :: String
    , fontDF       :: String
    , eventDF      :: String
    , extrasDF     :: String
    }

-- Create a dzen string with its flags
dzenFlagsToStr :: DF -> String
dzenFlagsToStr df =
    " -x '" ++ show (xPosDF df) ++
    "' -y '" ++ show (yPosDF df) ++
    "' -w '" ++ show (widthDF df) ++
    "' -h '" ++ show (heightDF df) ++
    "' -ta '" ++ alignementDF df ++
    "' -fg '" ++ fgColorDF df ++
    "' -bg '" ++ bgColorDF df ++
    "' -fn '" ++ fontDF df ++
    "' -e '" ++ eventDF df ++
    "' " ++ extrasDF df

-- Launch dzen through the system shell and return a Handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df

-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp = (fmap . fmap) (dzenBoxStyle bpp)


--------------------------------------------------------------------------------
-- HARDCODED LOGGERS
--------------------------------------------------------------------------------

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2

-- Label
labelL :: String -> Logger
labelL = return . return


-- Init version for Logger
initL :: Logger -> Logger
initL = (fmap . fmap) initNotNull


-- Concat a list of loggers
concatL :: [Logger] -> Logger
concatL = foldr (++!) (return (return ""))


-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL = foldr (\ x -> (++!) (x ++! labelL " ")) (return (return ""))


initNotNull :: String -> String
initNotNull [] = "0\n"
initNotNull xs = init xs


tailNotNull :: [String] -> [String]
tailNotNull [] = ["0\n"]
tailNotNull xs = tail xs


-- Convert the content of a file into a Logger
fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
    let readWithE f1 e1 p1 = E.catch (do
        contents <- readFile p1
        return $ f1 (initNotNull contents) ) ((\_ -> return e1) :: E.SomeException -> IO String)
    str <- liftIO $ readWithE f e p
    return $ return str

-- How strong the signal is
wifiStr :: Logger
wifiStr = fileToLogger format "N/A" "/proc/net/wireless" where
    format x = if length (lines x) >= 3 then initNotNull (words (lines x !! 2) !! 3) ++ " dBm" else "Off"

-- How good reception is (percentage of packages successful)
wifiPerc :: Logger
wifiPerc = fileToLogger format "N/A" "/proc/net/wireless" where
    format x = if length (lines x) >= 3 then initNotNull (words (lines x !! 2) !! 2) ++ "%" else "Off"

ramUsage :: [String -> String] -> Logger
ramUsage xs = initL $ concatWithSpaceL $ map funct xs where
    funct x = fileToLogger x "N/A" "/proc/meminfo"

memUsage :: Logger
memUsage = fileToLogger id "N/A" (logpath ++ "store/disk.txt")

cpuUsage :: String -> Int -> String -> Logger
cpuUsage path v c = fileToLogger format "0" path where
    format x = if null x then "N/A" else initNotNull $ concatMap ((++" ") . crit) (tailNotNull $ words x)
    crit x = if (read x::Int) >= v then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else x ++ "%"

-- Uptime
uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime" where
    u x  = read (takeWhile (/='.') x)::Integer
    h x  = div (u x) 3600
    format x = show (h x) ++ "h"

-- Gets the current resolution given a display and a screen
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
    dpy <- openDisplay d
    r <- liftIO $ getScreenInfo dpy
    closeDisplay dpy
    return Res
        { xRes = fromIntegral $ rect_width $ r !! n
        , yRes = fromIntegral $ rect_height $ r !! n
        }


npacSync :: Logger
npacSync = fileToLogger id "N/A" $ logpath ++ "pacman/pacsynccount.txt"


nmailSync :: String -> Logger
nmailSync c = fileToLogger format "N/A" $ logpath ++ "mail/mailsynccount.txt"
    where
        format x = if (read x::Int) >= 1 then "^fg(" ++ c ++ ")" ++ x ++ "^fg()" else x

timelog :: String -> Logger
timelog c = fileToLogger format "N/A" $ logpath ++ "timelog/active.txt"
    where
        format x = if (read x::Int) >= 1 then "^fg(" ++ c ++ ")" ++ x ++ "^fg()" else x

brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "N/A" "/sys/class/backlight/intel_backlight/actual_brightness" where
    format x = show (myround ((read x :: Float) / fromIntegral p) 1 * 100) ++ "%"
        where myround f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)


soundPerc :: Logger
soundPerc = do
    status <- fileToLogger id "N/A" $ logpath ++ "sound/soundstat.txt"
    let color s = if s == Just "on" then colorGreen else colorWhite
        format x = "^fg(" ++ color status ++ ")" ++ x ++ "^fg()"
    fileToLogger format "N/A" $ logpath ++ "sound/soundperc.txt"


batPercent :: String -> Int -> Logger
batPercent "keysersoze" v = do
    status <- fileToLogger id "N/A" "/sys/class/power_supply/BAT0/status"
    let color (Just "Charging") x                        = colorBlue
        color (Just "Discharging") x | (read x :: Int) < v = colorRed
        color _ _                                        = colorWhite
        format x = "^fg(" ++ color status x ++ ")" ++ x ++ "%^fg()"
    fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity"
