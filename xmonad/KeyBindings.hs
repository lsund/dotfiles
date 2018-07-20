
module KeyBindings where

import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Layout.Minimize
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
    , promptBorderWidth = 2
    , height            = 24
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

printScreen =
    spawn
        "sleep 3s && /usr/bin/scrot '%Y-%m-%d_$wx$h.png'" >>
        flashText myTextConfig 1 " Screen capture in 3 s "

--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------

-- Key bindings
myKeys hostname conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    -- Quit Xmonad
    [((modMask .|. shiftMask    , xK_q)         ,   killAndExit)
    -- Suspend
    , ((modMask                 , xK_End)       ,   spawn "systemctl suspend")
    -- Restart
    , ((modMask                 , xK_Home)      ,   killAndRestart)
    -- Run command
    , ((modMask                 , xK_r)         ,   shellPrompt myXPConfig)
    -- launch terminal
    , ((modMask                 , xK_Return)    ,   spawn $ XMonad.terminal conf)
    -- Kill window
    , ((modMask                 , xK_w)         ,   kill)
    -- next window
    , ((modMask                 , xK_j)         ,   windows W.focusDown)
    -- Prev window
    , ((modMask                 , xK_k)         ,   windows W.focusUp)
    -- Swap windows down
    , ((modMask .|. shiftMask   , xK_j)         ,   windows W.swapDown)
    -- Swap windows next
    , ((modMask .|. shiftMask   , xK_k)         ,   windows W.swapUp)
    -- Shrink master window horizontally
    , ((modMask                 , xK_h)         ,   sendMessage Shrink)
    -- Expand master area horizontally
    , ((modMask                 , xK_l)         ,   sendMessage Expand)
    -- Expand master area vertically
    , ((modMask .|. shiftMask   , xK_l)         ,   sendMessage MirrorExpand)
    -- Tiled mode
    , ((modMask                 , xK_f)         ,   withFocused $ windows . W.sink)
    -- Full screen
    , ((modMask .|. shiftMask   , xK_f)         ,   fullFloatFocused)
    -- Float
    , ((modMask .|. controlMask , xK_f)         ,   rectFloatFocused)
    -- Minimize
    -- , ((modMask                 , xK_m)         ,   withFocused minimizeWindow)
    -- Maximize
    -- , ((modMask .|. shiftMask   , xK_m)         ,   sendMessage RestoreNextMinimizedWin)
    -- Use next window layout
    , ((modMask                 , xK_space)     ,   sendMessage NextLayout)
    -- Scratchpad terminal
    , ((modMask                 , xK_s)         ,   scratchpadSpawnActionTerminal myTerminal)
    -- Take screenshot
    , ((0                       , xK_Print)     ,   printScreen)
    -- Toggle volume
    , ((modMask                 , xK_F1)        ,   spawn $ scriptpath ++ "soundcontrol toggle")
    -- Lower volume
    , ((modMask                 , xK_F2)        ,   spawn $ scriptpath ++ "soundcontrol lower")
    -- Raise volume
    , ((modMask                 , xK_F3)        ,   spawn $ scriptpath ++ "soundcontrol raise")
    -- Toggle active touchpad
    , ((modMask                 , xK_F8)        ,   spawn $ scriptpath ++ "toggle-touchpad")
    -- Mark mail as read
    , ((modMask                 , xK_F9)        ,   spawn $ scriptpath ++ "mailsync update")
    -- Decrease backlight
    , ((modMask                 , xK_F11)       ,   spawn "/usr/bin/xbacklight -dec 10")
    -- Raise backlight
    , ((modMask                 , xK_F12)       ,   spawn "/usr/bin/xbacklight -inc 10")

    -- Change keyboard layout
    , ((modMask                 , xK_F5)       ,   spawn "/usr/bin/setxkbmap us")
    , ((modMask                 , xK_F6)       ,   spawn "/usr/bin/setxkbmap dvorak")
    ]
    ++
    [
        ((m .|. modMask, k), windows $ f i) |
         (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    [ ((m .|. modMask .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_j, xK_k, xK_F2] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ] where
        fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
        rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.05 0.05 0.9 0.9) f
        killAndExit =   spawn
                            "/usr/bin/killall dzen2 haskell-cpu-usage.out" <+>
                            io exitSuccess
        killAndRestart = spawn
                            "/usr/bin/killall dzen2 haskell-cpu-usage.out" <+>
                            liftIO (threadDelay 2000000) <+>
                            restart "xmonad" True

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) ]
