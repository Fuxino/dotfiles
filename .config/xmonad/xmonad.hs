import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (xmobar1 <> xmobar2) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask               = mod4Mask
    , terminal              = "kitty"
    , focusedBorderColor    = "#000000"
    , normalBorderColor     = "#ffffff"
    , layoutHook            = myLayout
    , startupHook           = myStartupHook
    , manageHook            = myManageHook
    }
    `additionalKeysP`
    [ ("M-S-l"      , spawn "xscreensaver-command -lock"    )
    , ("M-v"        , spawn "vivaldi"                       )
    , ("M-<Print>"  , unGrab *> spawn "gnome-screenshot -i" )
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
    where
        threeCol
            = renamed [Replace "ThreeCol"]
            $ magnifiercz' 1.3
            $ ThreeColMid nmaster delta ratio
        tiled       = Tall nmaster delta ratio
        nmaster     = 1
        ratio       = 1/2
        delta       = 3/100

xmobar1 = statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc"   (pure myXmobarPP)
xmobar2 = statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc"   (pure myXmobarPP)

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

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "mons -e left"
    spawnOnce "xscreensaver -no-splash"
    spawnOnce "redshift"
    spawnOnce "dunst"
    spawnOnce "~/.fehbg"

myManageHook ::  ManageHook
myManageHook = composeAll
    [ className =? "mpv"    --> doFullFloat
    , isDialog              --> doFloat
    ]
