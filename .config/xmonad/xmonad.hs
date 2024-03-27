-- Import
import XMonad as X

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
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
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed

-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- Extra keys
import Graphics.X11.ExtraTypes.XF86

import XMonad.StackSet as W


-- Main
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (xmobar1 <> xmobar2) defToggleStrutsKey
     $ myConfig

-- Variables
myModMask   = mod4Mask

myTerminal :: String
myTerminal  = "kitty"

-- Config
myConfig = def
    { modMask               = myModMask
    , terminal              = myTerminal
    , X.workspaces          = myWorkspaces
    , focusedBorderColor    = "#006700"
    , normalBorderColor     = "#000000"
    , layoutHook            = myLayout
    , startupHook           = myStartupHook
    , manageHook            = myManageHook
    }
    `additionalKeysP`
    [ ("M-S-l"      , spawn "xscreensaver-command -lock"    )
    , ("M-<Print>"  , unGrab *> spawn "gnome-screenshot -i" )
    , ("M-d"        , spawn "dmenu_run"                     )
    , ("M-p"        , spawn "passmenu -i"                   )
    , ("M-f"        , sendMessage $ JumpToLayout "Tabbed"   )
    ]
    `additionalKeys`
    [ ((0, xF86XK_AudioMute)        , spawn "pactl set-sink-mute $(pactl get-default-sink) toggle"  )
    , ((0, xF86XK_AudioLowerVolume) , spawn "pactl set-sink-volume $(pactl get-default-sink) -10%"  )
    , ((0, xF86XK_AudioRaiseVolume) , spawn "pactl set-sink-volume $(pactl get-default-sink) +10%"  )
    , ((0, xF86XK_Calculator)       , spawn "galculator"                                            )
    , ((0, xF86XK_HomePage)         , spawn "vivaldi"                                               )
    , ((0, xF86XK_Mail)             , spawn "kitty mutt"                                            )
    , ((0, xF86XK_Tools)            , spawn "kitty ncmpcpp"                                         )
    ]

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = [ "1:\xf489 ", "2:\xe743 ", "3:\xf1b6 ", "4:\xf10b ", "5:\xead9 " ] ++ map show [6..9]

-- Layout
myLayout = onWorkspace "2:\xe743 " myWebLayout $ onWorkspace "3:\xf1b6 " myGamesLayout $ myDefaultLayout
    where
        myWebLayout = avoidStruts $ myTabbed ||| tiled ||| Mirror tiled ||| threeCol ||| Grid ||| spiral(0.856)
        myGamesLayout = avoidStruts $ Full
        myDefaultLayout = avoidStruts $ tiled ||| Mirror tiled ||| myTabbed ||| threeCol ||| Grid ||| spiral(0.856)
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
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

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
    [ className =? "kitty"              --> doShift "1:\xf489 "
    , className =? "Vivaldi-stable"     --> doShift "2:\xe743 "
    , className =? "steam"              --> doShift "3:\xf1b6 "
    , className =? "discord"            --> doShift "4:\xf10b "
    , className =? "Signal"             --> doShift "4:\xf10b "
    , className =? "mpv"                --> doFullFloat
    , className =? "mpv"                --> doShift "5:\xead9 "
    , className =? "Gpodder"            --> doShift "5:\xead9 "
    , className =? "Xviewer"            --> doFloat
    , className =? "Galculator"         --> doFloat
    , className =? "steam_app_109600"   --> doFloat
    , className =? "Xmessage"           --> doFloat
    , className =? "Windscribe2"        --> doShift "9"
    , className =? "Windscribe2"        --> doRectFloat (W.RationalRect 0.4 0.4 0.6 0.6)
    , isDialog                          --> doFloat
    , isFullscreen                      --> doFullFloat
    ]
