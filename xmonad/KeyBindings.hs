
module KeyBindings where

import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Layout.Minimize
import XMonad.Layout.IndependentScreens
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.ShowText
import Data.Monoid
import System.Exit
import Control.Concurrent
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.Scratchpad

import Config

myXPConfig :: XPConfig
myXPConfig = def
    { font              = dzenFont
    , bgColor           = colorBlack
    , fgColor           = colorWhite
    , bgHLight          = colorBlue
    , fgHLight          = colorBlack
    , borderColor       = colorGrayAlt
    , promptBorderWidth = 1
    , height            = 16
    , position          = Top
    , historySize       = 100
    , historyFilter     = deleteConsecutive
    , autoComplete      = Nothing
    }

myTextConfig :: ShowTextConfig
myTextConfig = STC
    { st_font = dzenFont
    , st_bg   = colorBlack
    , st_fg   = colorWhite
    }

--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------

-- Key bindings
myKeys hostname conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    -- Quit Xmonad
    [((modMask .|. shiftMask, xK_q), killAndExit)
    -- Suspend
    , ((modMask, xK_End), spawn "systemctl suspend")
    -- Restart
    , ((modMask, xK_Home), killAndRestart)
    -- Run command
    , ((modMask, xK_r), shellPrompt myXPConfig)
    -- launch terminal
    , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    -- Kill window
    , ((modMask, xK_w), kill)
    -- next window
    , ((modMask, xK_j), windows W.focusDown)
    -- Prev window
    , ((modMask, xK_k), windows W.focusUp)
    -- Swap windows down
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap windows next
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink master window horizontally
    , ((modMask, xK_h), sendMessage Shrink)
    -- Expand master area horizontally
    , ((modMask, xK_l), sendMessage Expand)
    -- Shrink master area vertically 
    , ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
    -- Expand master area vertically 
    , ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand)
    -- Push window back into tiled mode
    , ((modMask, xK_f), withFocused $ windows . W.sink)
    -- Push window into float
    , ((modMask .|. controlMask, xK_f), rectFloatFocused)
    -- Minimize window
    , ((modMask, xK_m), withFocused minimizeWindow)
    -- Maximize
    , ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    -- Full screen window
    , ((modMask .|. shiftMask, xK_f), fullFloatFocused)
    -- Use next window layout
    , ((modMask, xK_space), sendMessage NextLayout)
    -- Screenshot binding
    , ((modMask, xK_s), scratchpadSpawnActionTerminal myTerminal)
    , (
        (0, xK_Print)
        , spawn
            "/usr/bin/scrot '%Y-%m-%d_$wx$h.png'" >>
            flashText myTextConfig 1 " Screenshot Saved ")
    ] ++ (
    if hostname == "keysersoze"
        then
    [
      ((modMask, xK_F1), spawn $ automaticScriptPath ++ "soundcontrol toggle")
    , ((modMask, xK_F2), spawn $ automaticScriptPath ++ "soundcontrol lower")
    , ((modMask, xK_F3), spawn $ automaticScriptPath ++ "soundcontrol raise")
    , ((modMask, xK_F8), spawn $ automaticScriptPath ++ "toggle-touchpad")
    , ((modMask, xK_F9), spawn $ automaticScriptPath ++ "mailsync update")
    , ((modMask, xK_F11), spawn "/usr/bin/xbacklight -dec 10")
    , ((modMask, xK_F12), spawn "/usr/bin/xbacklight -inc 10")
    ] else
    [
      ((modMask, xK_F2), spawn "/usr/bin/xbacklight -dec 10")
    , ((modMask, xK_F3), spawn "/usr/bin/xbacklight -inc 10")
    , ((modMask, xK_F6), spawn $ automaticScriptPath ++ "soundcontrol toggle")
    , ((modMask, xK_F7), spawn $ automaticScriptPath ++ "soundcontrol lower")
    , ((modMask, xK_F8), spawn $ automaticScriptPath ++ "soundcontrol raise")
    , ((modMask, xK_F5), spawn $ automaticScriptPath ++ "toggle-touchpad")
    , ((modMask, xK_F9), spawn $ automaticScriptPath ++ "mailsync update")
    ]) ++
    if hostname == "dennis" || hostname == "keysersoze"
        then
            [ ((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
            ]
        else
            [ ((m .|. modMask, k), windows $ onCurrentScreen f i)
            | (i, k) <- zip (workspaces' conf) ([xK_1 .. xK_9] ++ [xK_0])
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
            ]
    ++
    [ ((m .|. modMask .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_j, xK_k, xK_F2] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ] where
        fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
        rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.05 0.05 0.9 0.9) f
        killAndExit =
            spawn "/usr/bin/killall dzen2 haskell-cpu-usage.out" <+>
            io exitSuccess
        killAndRestart =
            spawn "/usr/bin/killall dzen2 haskell-cpu-usage.out" <+>
            liftIO (threadDelay 2000000) <+>
            restart "xmonad" True

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) ]


