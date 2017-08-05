
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
import XMonad.Util.NamedScratchpad
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

-- Main
main :: IO ()
main = do
    hostname' <- readFile "/etc/hostname"
    let hostname = init hostname'
    r <- getScreenRes ":0" 0  --display ":0", screen 0
    topLeftBar  <- dzenSpawnPipe $ dzenTopLeftFlags r
    topRightBar <- dzenSpawnPipe $ dzenTopRightFlags r
    botLeftBar  <- dzenSpawnPipe $ dzenBotLeftFlags r
    botRightBar <- dzenSpawnPipe $ dzenBotRightFlags r
    xmonad $ def
        { terminal           = "/usr/bin/urxvt"                 --default terminal
        , modMask            = mod4Mask                         --default modMask
        , focusFollowsMouse  = True                             --focus follow config
        , clickJustFocuses   = True                             --focus click config
        , borderWidth        = 1                                --border width
        , normalBorderColor  = colorBlackAlt                    --border color
        , focusedBorderColor = colorWhite                       --focused border color
        , workspaces         = myWorkspaces hostname            --workspace names
        , startupHook        = myStartupHook                    --autostart config
        , handleEventHook    = myHandleEventHook                --event config
        , layoutHook         = myLayoutHook hostname            --layout config
        , manageHook         = myManageHook hostname            --xprop config
        , logHook            =                                  --status bar config
            myTopLeftLogHook topLeftBar hostname  <+>                   --top left dzen
            myTopRightLogHook topRightBar <+>                   --top right dzen
            myBotLeftLogHook botLeftBar hostname   <+>          --bottom left dzen
            myBotRightLogHook botRightBar hostname <+>          --bottom right dzen
            ewmhDesktopsLogHook           >>
            setWMName "LG3D"
        , keys               = myKeys hostname                  --key bindings config
        , mouseBindings      = myMouseBindings                  --mouse bindings config
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
    , decoHeight          = 14
    }

-- Prompt theme
-- Flash text config
myTextConfig :: ShowTextConfig
myTextConfig = STC
    { st_font = dzenFont
    , st_bg   = colorBlack
    , st_fg   = colorWhite
    }

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

blueBoxPP :: BoxPP
blueBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlue
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon
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

whiteBoxPP :: BoxPP
whiteBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorWhiteAlt
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

grayBoxPP :: BoxPP
grayBoxPP = BoxPP
    { bgColorBPP   = colorGray
    , fgColorBPP   = colorGray
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

blackBoxPP :: BoxPP
blackBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorGrayAlt
    , leftIconBPP  = boxLeftIcon
    , rightIconBPP = boxRightIcon
    , boxHeightBPP = boxHeight
    }

white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
    { bgColorBPP   = colorBlack
    , fgColorBPP   = colorBlack
    , boxColorBPP  = colorWhiteAlt
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

layoutCA :: CA
layoutCA = CA
    { leftClickCA   = "/usr/bin/xdotool key super+space"
    , middleClickCA = "/usr/bin/xdotool key super+v"
    , rightClickCA  = "/usr/bin/xdotool key super+shift+space"
    , wheelUpCA     = "/usr/bin/xdotool key super+f"
    , wheelDownCA   = "/usr/bin/xdotool key super+control+f"
    }

workspaceCA :: CA
workspaceCA = CA
    { leftClickCA   = "/usr/bin/xdotool key super+1"
    , middleClickCA = "/usr/bin/xdotool key super+g"
    , rightClickCA  = "/usr/bin/xdotool key super+0"
    , wheelUpCA     = "/usr/bin/xdotool key ctrl+alt+Right"
    , wheelDownCA   = "/usr/bin/xdotool key ctrl+alt+Left"
    }

focusCA :: CA
focusCA = CA
    { leftClickCA   = "/usr/bin/xdotool key super+m"
    , middleClickCA = "/usr/bin/xdotool key super+c"
    , rightClickCA  = "/usr/bin/xdotool key super+shift+m"
    , wheelUpCA     = "/usr/bin/xdotool key super+shift+j"
    , wheelDownCA   = "/usr/bin/xdotool key super+shift+k"
    }

every n xs = case drop (n - 1) xs of
              (y : ys) -> y : every n ys
              [] -> []

myWorkspaces hostname =
    if hostname == "dennis" || hostname == "keysersoze"
        then map show [1..9]
        else every 2 ("0" : screenList) ++ every 2 screenList
    where screenList = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Workspace names
workspaceNames :: [WorkspaceId]
workspaceNames =
    [ "Term"
    , "Browser"
    , "App1"
    , "App2"
    , "App3"
    , "App4"
    , "App5"
    , "App6"
    , "App7"
    , "App8"
    ]

-- Layout names 
myTileName = "Tiled"
myMosAName = "Mosaic"
myCst3Name = "Web"

myFTabName = "Full"
myFloaName = "Float"


--------------------------------------------------------------------------------------------
-- STARTUP HOOK CONFIG                                                                    --
--------------------------------------------------------------------------------------------

-- Startup Hook
myStartupHook =
    setDefaultCursor xC_left_ptr <+>
    spawn "feh --bg-scale /home/lsund/Media/image/haskell.png &" <+>
    spawn "/usr/bin/killall haskell-cpu-usage.out" <+>
    liftIO (threadDelay 1000000) <+> --needed so that xmonad can be launched on the fly without crashing
    spawn "xrandr --output DVI-I-1 --right-of HDMI-0 --output HDMI-0 --primary" <+>
    spawn "/home/lsund/.xmonad/apps/haskell-cpu-usage.out 5" <+>
    (startTimer 1 >>= XS.put . TID)


--------------------------------------------------------------------------------------------
-- HANDLE EVENT HOOK CONFIG                                                               --
--------------------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------

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

-- Layout hook
myLayoutHook hostname =
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
      myToggleL myMosA myMosAName ||| myToggleL myCst3 myCst3Name
        --layouts
    myTile = ResizableTall 1 0.03 0.5 [] --default xmonad layout
    myMosA = MosaicAlt M.empty --default mosaicAlt layout
    myCst3 =
      layoutN 1 (relBox 0 0 1 0.7) (Just $ relBox 0 0 1 1) myTabb $
        layoutAll (relBox 0 0.7 1 1) myTabb
    myTabb = tabbed shrinkText myTitleTheme
        --custom draggingVisualizer toggle
    myToggleL l n =
      smartBorders $
      toggleLayouts
        (named ("Switcher " ++ n) $ switcher l)
        (named ("Normal " ++ n) l)
      where
        switcher l =
          windowSwitcherDecoration shrinkText myTitleTheme $
          draggingVisualizer l

--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- Manage Hook
myManageHook :: String -> ManageHook
myManageHook hostname =
    manageDocks <+>
    dynamicMasterHook <+>
    manageWindows hostname

-- Manage Windows
manageWindows :: String -> ManageHook
manageWindows hostname = composeAll . concat $
    [ [ resource  =? r --> doIgnore                    | r <- myIgnores ]
    , [ className =? c --> doCenterFloat               | c <- myFloatCC ]
    , [ name      =? n --> doCenterFloat               | n <- myFloatCN ]
    , [ name      =? n --> doSideFloat NW              | n <- myFloatSN ]
    , [ className =? c --> doF W.focusDown             | c <- myFocusDC ]
    , [ currentWs =? (myWorkspaces hostname !! 1) --> keepMaster "Chromium"      ]
    , [ isFullscreen   --> doFullFloat ]
    ] where
        name      = stringProperty "WM_NAME"
        myIgnores = ["desktop", "desktop_window"]
        myWebS    = ["Chromium", "Firefox", "Opera"]
        myChatS   = ["Pidgin", "Xchat"]
        {-myGfxS    = ["Gimp", "gimp", "GIMP"]-}
        myAlt3S   = ["Amule", "Transmission-gtk"]
        myFloatCC = ["MPlayer", "mplayer2", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1"
                    , "Gksu", "Galculator", "Nvidia-settings", "XFontSel", "XCalc", "XClock"
                    , "Ossxmix", "Xvidcap", "Main", "Wicd-client.py"]
        myFloatCN = ["Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences"
                    , "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions"
                    , "Autofill Options", "Rename File", "Copying files", "Moving files"
                    , "File Properties", "Replace", "Quit GIMP", "Change Foreground Color"
                    , "Change Background Color", ""]
        myFloatSN = ["Event Tester"]
        myFocusDC = ["Event Tester", "Notify-osd"]
        keepMaster c = assertSlave <+> assertMaster where
            assertSlave = fmap (/= c) className --> doF W.swapDown
            assertMaster = className =? c --> doF W.swapMaster

--------------------------------------------------------------------------------------------
-- DZEN STATUS BARS CONFIG                                                                --
--------------------------------------------------------------------------------------------

-- Dzen top left bar flags
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
  { ppOutput = hPutStrLn h
  , ppOrder = \(ws:_:_:x) -> ws : x
  , ppSep = " "
  , ppWsSep = ""
  , ppCurrent = dzenBoxStyle blue2BBoxPP
  , ppUrgent = dzenBoxStyle red2BBoxPP . dzenClickWorkspace
  , ppVisible = dzenBoxStyle green2BBoxPP . dzenClickWorkspace
  , ppHiddenNoWindows = dzenBoxStyle gray2BBoxPP . dzenClickWorkspace
  , ppHidden = dzenBoxStyle whiteBoxPP . dzenClickWorkspace
  , ppExtras = [ myFocusL ]
  }
  where
    dzenClickWorkspace ws =
      "^ca(1," ++
      xdo "w;" ++
      xdo index ++
      ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()"
      where
        wsIdxToString Nothing = "1"
        wsIdxToString (Just n) =
          show $ mod (n + 1) (2 * length (myWorkspaces hostname))
        index = wsIdxToString (elemIndex ws (myWorkspaces hostname))
        xdo key = "/usr/bin/xdotool key super+" ++ key

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
    , ppExtras  = [ myDateL ]
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
  dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $
  def
  { ppOutput = hPutStrLn h
  , ppOrder = \(_:_:_:x) -> x
  , ppSep = " "
  , ppExtras = [myUptimeL, myMemL, myWifiL, myCpuL]
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
        [myPacSyncL, myMailSyncL, myBrightL hostname, mySoundL, myBatL hostname]
    }

--------------------------------------------------------------------------------------------
-- LOGGERS CONFIG                                                                         --
--------------------------------------------------------------------------------------------

myBatL hostname =
    dzenBoxStyleL blue2BoxPP (labelL "⚡") ++!
    dzenBoxStyleL whiteBoxPP (batStatus hostname) ++!
    dzenBoxStyleL whiteBoxPP  (batPercent hostname 30 colorRed)

myMemL =
    dzenBoxStyleL blue2BoxPP (labelL "•") ++!
    dzenBoxStyleL whiteBoxPP (memUsage [freeBMemUsage])
        where freeBMemUsage x =
                let bytes     = _memValues x !! 2
                in show (div bytes 1000) ++ "M"

myCpuL =
    dzenBoxStyleL blue2BoxPP (labelL "•") ++!
    dzenBoxStyleL whiteBoxPP (cpuUsage "/tmp/haskell-cpu-usage.txt" 70 colorRed)

myWifiL =
    dzenBoxStyleL blue2BoxPP (labelL "•") ++!
    dzenBoxStyleL whiteBoxPP wifiPerc ++!
    dzenBoxStyleL whiteBoxPP wifiStr

myBrightL hostname =
    dzenBoxStyleL blue2BoxPP (labelL "☼") ++!
    if hostname == "dennis" then
        dzenBoxStyleL whiteBoxPP $ brightPerc 4648
    else if hostname == "keysersoze" then
        dzenBoxStyleL whiteBoxPP $ brightPerc 937
    else
        dzenBoxStyleL whiteBoxPP $ brightPerc 1000
mySoundL =
    dzenBoxStyleL blue2BoxPP (labelL "♬") ++!
    dzenBoxStyleL whiteBoxPP soundPerc ++!
    dzenBoxStyleL whiteBoxPP soundStat

myPacSyncL =
    dzenBoxStyleL blue2BoxPP (labelL "♼") ++!
    dzenBoxStyleL whiteBoxPP npacSync

myMailSyncL = do
    logger <- nmailSync colorRed
    case logger of
        Just s ->
            if (read (getRaw s) :: Int) >= 1 then
                dzenBoxStyleL red2BBoxPP (labelL "✉") ++! dzenBoxStyleL whiteBoxPP (nmailSync colorRed)
            else
                dzenBoxStyleL blue2BoxPP (labelL "✉") ++! dzenBoxStyleL whiteBoxPP (nmailSync colorRed)
        Nothing -> dzenBoxStyleL blue2BoxPP (labelL "✉") ++! dzenBoxStyleL whiteBoxPP (nmailSync colorRed)

getRaw [x] = [x]
getRaw (')' : x : xs) = [x]
getRaw (x : xs) = getRaw xs


-- TopRight Loggers
myDateL =
    dzenBoxStyleL white2BBoxPP (date "%A") ++!
    dzenBoxStyleL whiteBoxPP   (date $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg()") ++!
    dzenBoxStyleL whiteBoxPP   (date $ "%H^fg(" ++ colorGray ++ "):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()")
myUptimeL =
    dzenBoxStyleL blue2BoxPP   (labelL "◷") ++!
    dzenBoxStyleL whiteBoxPP uptime

myFocusL = dzenBoxStyleL whiteBoxPP (shortenL 100 logTitle)

--------------------------------------------------------------------------------------------
-- DZEN UTILS                                                                             --
--------------------------------------------------------------------------------------------

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

-- Dzen box pretty config
data BoxPP = BoxPP
    { bgColorBPP   :: String
    , fgColorBPP   :: String
    , boxColorBPP  :: String
    , leftIconBPP  :: String
    , rightIconBPP :: String
    , boxHeightBPP :: Int
    }

-- Dzen clickable area config
data CA = CA
    { leftClickCA   :: String
    , middleClickCA :: String
    , rightClickCA  :: String
    , wheelUpCA     :: String
    , wheelDownCA   :: String
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

-- Uses dzen format to make dzen text clickable
dzenClickStyle :: CA -> String -> String
dzenClickStyle ca t = "^ca(1," ++ leftClickCA ca ++
    ")^ca(2," ++ middleClickCA ca ++
    ")^ca(3," ++ rightClickCA ca ++
    ")^ca(4," ++ wheelUpCA ca ++
    ")^ca(5," ++ wheelDownCA ca ++
    ")" ++ t ++
    "^ca()^ca()^ca()^ca()^ca()"

-- Launch dzen through the system shell and return a Handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df

-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp = (fmap . fmap) (dzenBoxStyle bpp)

-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca = (fmap . fmap) (dzenClickStyle ca)


--------------------------------------------------------------------------------------------
-- HARDCODED LOGGERS 
--------------------------------------------------------------------------------------------

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
{-concatL [] = return $ return ""-}
{-concatL (x:xs) = x ++! concatL xs-}

-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL = foldr (\ x -> (++!) (x ++! labelL " ")) (return (return ""))
{-concatWithSpaceL [] = return $ return ""-}
{-concatWithSpaceL (x:xs) = x ++! (labelL " ") ++! concatWithSpaceL xs-}

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

batPercent :: String -> Int -> String -> Logger
batPercent hostname v c
    | hostname == "keysersoze" = fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity"
    | hostname == "dennis" = fileToLogger format "N/A" "/sys/class/power_supply/BAT1/capacity"
    where
        format x = if (read x::Int) <= v then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else x ++ "%"

batStatus :: String -> Logger
batStatus "keysersoze" = fileToLogger id "N/A" "/sys/class/power_supply/BAT0/status"
batStatus "dennis" = fileToLogger id "N/A" "/sys/class/power_supply/BAT1/status"

brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "N/A" "/sys/class/backlight/intel_backlight/actual_brightness" where
    format x = show (myround ((read x :: Float) / fromIntegral p) 1 * 100) ++ "%"
        where myround f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

npacSync :: Logger
npacSync = fileToLogger id "N/A" $ logpath ++ "pacman/pacsynccount.txt"

nmailSync :: String -> Logger
nmailSync c = fileToLogger format "N/A" $ logpath ++ "mail/mailsynccount.txt"
    where
        format x = if (read x::Int) >= 1 then "^fg(" ++ c ++ ")" ++ x ++ "^fg()" else x

soundPerc :: Logger
soundPerc = fileToLogger id "0" $ logpath ++ "sound/soundperc.txt"

soundStat :: Logger
soundStat = fileToLogger id "Status" $ logpath ++ "sound/soundstat.txt"

wifiStr :: Logger
wifiStr = fileToLogger format "N/A" "/proc/net/wireless" where
    format x = if length (lines x) >= 3 then initNotNull (words (lines x !! 2) !! 3) ++ " dBm" else "Off"

wifiPerc :: Logger
wifiPerc = fileToLogger format "N/A" "/proc/net/wireless" where
    format x = if length (lines x) >= 3 then initNotNull (words (lines x !! 2) !! 2) ++ "%" else "Off"

memUsage :: [String -> String] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
    funct x = fileToLogger x "N/A" "/proc/meminfo"

_memValues x = map (getValues x) $ take 4 [0..] where
    getValues x n = read (words (lines x !! n) !! 1) :: Int

cpuUsage :: String -> Int -> String -> Logger
cpuUsage path v c = fileToLogger format "0" path where
    format x = if null x then "N/A" else initNotNull $ concatMap ((++" ") . crit) (tailNotNull $ words x)
    crit x = if (read x::Int) >= v then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else x ++ "%"

-- Uptime
uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime" where
    u x  = read (takeWhile (/='.') x)::Integer
    h x  = div (u x) 3600
    hr x = mod (u x) 3600
    m x  = div (hr x) 60
    s x  = mod (hr x) 60
    format x = show (h x) ++ "h " ++ show (m x) ++ "m " ++ show (s x) ++ "s"

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

-- Screen Resolution
data Res = Res { xRes :: Int, yRes :: Int }

-- Resolution logger
screenRes :: String -> Int -> Logger
screenRes d n = do
    res <- liftIO $ getScreenRes d n
    return $ return $ show (xRes res) ++ "x" ++ show (yRes res)


