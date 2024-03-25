-- Import
import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layout modifiers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName

-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- Main
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (xmobar1 <> xmobar2) defToggleStrutsKey
     $ myConfig

-- Config
myConfig = def
    { modMask               = mod4Mask
    , terminal              = "kitty"
    , workspaces            = myWorkspaces
    , focusedBorderColor    = "#006700"
    , normalBorderColor     = "#000000"
    , layoutHook            = showWName myLayout
    , startupHook           = myStartupHook
    , manageHook            = myManageHook
    }
    `additionalKeysP`
    [ ("M-S-l"      , spawn "xscreensaver-command -lock"    )
    , ("M-v"        , spawn "vivaldi"                       )
    , ("M-<Print>"  , unGrab *> spawn "gnome-screenshot -i" )
    , ("M-d"        , spawn "dmenu_run"                     )
    , ("M-p"        , spawn "passmenu -i"                   )
    , ("M-f"        , sendMessage $ JumpToLayout "Tabbed"   )
    ]

-- Workspaces
myWorkspaces = [ "1:term", "2:www", "3:games", "4:msg" ] ++ map show [5..9]

-- Layout
myLayout = tiled ||| Mirror tiled ||| myTabbed ||| threeCol ||| Grid ||| spiral(0.856)
    where
        threeCol
            = renamed [Replace "ThreeCol"]
            $ magnifiercz' 1.3
            $ ThreeColMid nmaster delta ratio
        myTabbed
            = renamed [Replace "Tabbed"]
            $ tabbed shrinkText myTabConfig
        myTabConfig = def { activeColor     = "#393939"
                          , inactiveColor   = "#191b1c"
                          }
        tiled       = Tall nmaster delta ratio
        nmaster     = 1
        ratio       = 1/2
        delta       = 3/100

-- Xmobar
xmobar1 = statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc_laptop"    (pure myXmobarPP)
xmobar2 = statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc_hdmi"  (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = darkgreen " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
    where
        formatFocused   = wrap (white       "[") (white     "]") . darkgreen    . ppWindow
        formatUnfocused = wrap (lowWhite    "[") (lowWhite  "]") . white        . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, darkgreen, red, white, yellow :: String -> String
        darkgreen   = xmobarColor "#013220" ""
        blue        = xmobarColor "#bd93f9" ""
        white       = xmobarColor "#f8f8f2" ""
        yellow      = xmobarColor "#f1fa8c" ""
        red         = xmobarColor "#ff5555" ""
        lowWhite    = xmobarColor "#bbbbbb" ""

-- Autostart
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "mons -e left && ~/.fehbg"
    spawnOnce "xscreensaver -no-splash"
    spawnOnce "trayer --edge top --align right --SetDockType true \
              \--SetPartialStrut true --expand true --width 7 \
              \--transparent true --tint 0x1f2022 --height 18 \
              \--monitor 0"
    spawnOnce "redshift-gtk"
    spawnOnce "udiskie"
    spawnOnce "nm-applet"
    spawnOnce "discover-overlay"
    spawnOnce "arch-audit-gtk"
    spawnOnce "xcompmgr"

-- Manage hook
myManageHook ::  ManageHook
myManageHook = composeAll
    [ insertPosition End Newer
    , className =? "kitty"              --> doShift "1:term"
    , className =? "Vivaldi-stable"     --> doShift "2:www"
    , className =? "steam"              --> doShift "3:games"
    , className =? "discord"            --> doShift "4:msg"
    , className =? "Signal"             --> doShift "4:msg"
    , className =? "mpv"                --> doFullFloat
    , className =? "Xviewer"            --> doFloat
    , className =? "steam_app_109600"   --> doFloat
    , isDialog                          --> doFloat
    , isFullscreen                      --> doFullFloat
    ]
