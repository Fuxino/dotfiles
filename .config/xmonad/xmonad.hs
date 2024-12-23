-- Import

import Data.Default
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatConfigureReq
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- Main
main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (xmobar1 <> xmobar2) defToggleStrutsKey
    $ myConfig

-- Variables
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myBrowser :: String
myBrowser = "vivaldi"

myNormBorderColor :: String
myNormBorderColor = "#1f2335"

myFocusBorderColor :: String
myFocusBorderColor = "#41a6b5"

myKeyboardLayoutIT :: String
myKeyboardLayoutIT = " -layout it"

myKeyboardLayoutUS = " -layout us"

myKeyboardLayoutSK = " -layout sk -variant qwerty"

myScreenshotDir :: String
myScreenshotDir = "/home/fuxino/Pictures/Screenshots/"

myScreenshotName :: String
myScreenshotName = "Screenshot-%Y-%m-%d-%H%M%S.png"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "Windscribe" "windscribe" (className =? "Windscribe2") (customFloating $ W.RationalRect 0.4 0.4 0.6 0.6),
    NS "Terminal" spawnTerminal (title =? "kitty-float") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6),
    NS "Music" spawnMusic (title =? "ncmpcpp") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6),
    NS "Mail" spawnMail (title =? "mutt") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6),
    NS "Cal" spawnCal (title =? "calcurse") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6),
    NS "Btop" spawnBtop (title =? "btop") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
  ]
  where
    spawnTerminal = myTerminal ++ " -T kitty-float"
    spawnMusic = myTerminal ++ " -T ncmpcpp ncmpcpp"
    spawnMail = myTerminal ++ " -T mutt bash -c 'pushd ~/Downloads > /dev/null; mutt; popd > /dev/null'"
    spawnCal = myTerminal ++ " -T calcurse calcurse"
    spawnBtop = myTerminal ++ " -T btop btop"

-- Config
myConfig =
  def
    { modMask = myModMask,
      terminal = myTerminal,
      workspaces = myWorkspaces,
      focusedBorderColor = myFocusBorderColor,
      normalBorderColor = myNormBorderColor,
      layoutHook = myLayoutHook,
      startupHook = myStartupHook,
      manageHook = myManageHook,
      handleEventHook = myHandleEventHook
    }
    `additionalKeysP` [ ("M-S-l", spawn "slock"),
                        ("<Print>", unGrab *> spawn ("scrot " ++ myScreenshotDir ++ myScreenshotName)),
                        ("S-<Print>", unGrab *> spawn ("scrot -s " ++ myScreenshotDir ++ myScreenshotName)),
                        ("M-S-<Print>", unGrab *> spawn ("scrot -u " ++ myScreenshotDir ++ myScreenshotName)),
                        ("M-d", spawn "dmenu_run"),
                        ("M-p", spawn "passmenu -i"),
                        ("M-f", sendMessage $ JumpToLayout "Tabbed"),
                        ("M-i", spawn $ "setxkbmap" ++ myKeyboardLayoutIT),
                        ("M-u", spawn $ "setxkbmap" ++ myKeyboardLayoutUS),
                        ("M-s", spawn $ "setxkbmap" ++ myKeyboardLayoutSK),
                        ("M-S-v", namedScratchpadAction myScratchpads "Windscribe"),
                        ("M-S-s", namedScratchpadAction myScratchpads "Terminal"),
                        ("M-S-a", namedScratchpadAction myScratchpads "Cal"),
                        ("M-o", namedScratchpadAction myScratchpads "Btop"),
                        ("M-x", spawn "bluetoothctl connect E8:EE:CC:3E:A6:0D"),
                        ("M-S-x", spawn "bluetoothctl disconnect E8:EE:CC:3E:A6:0D")
                      ]
    `additionalKeys` [ ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute $(pactl get-default-sink) toggle"),
                       ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) -10%"),
                       ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) +10%"),
                       ((0, xF86XK_Calculator), spawn "qalculate-gtk"),
                       ((0, xF86XK_HomePage), spawn myBrowser),
                       ((0, xF86XK_Mail), namedScratchpadAction myScratchpads "Mail"),
                       ((0, xF86XK_Tools), namedScratchpadAction myScratchpads "Music")
                     ]
    ++ [ ((myModMask .|. mask, key), f sc)
         | (key, sc) <- zip [xK_w, xK_e] [0 ..],
           (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
       ]

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = ["1:\xf489 ", "2:\xe743 ", "3:\xf1b6 ", "4:\xf10b ", "5:\xead9 ", "6:\xeb69 "] ++ map show [7 .. 9]

-- Layout
myLayoutHook = onWorkspace "2:\xe743 " myWebLayout $ onWorkspace "3:\xf1b6 " myGamesLayout myDefaultLayout
  where
    myWebLayout = avoidStruts $ smartBorders $ myTabbed ||| tiled ||| Mirror tiled ||| threeCol ||| Grid ||| spiral 0.856
    myGamesLayout = avoidStruts $ smartBorders Full
    myDefaultLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| myTabbed ||| threeCol ||| Grid ||| spiral 0.856
    threeCol =
      renamed [Replace "ThreeCol"] $
        magnifiercz' 1.3 $
          ThreeColMid nmaster delta ratio
    myTabbed =
      renamed [Replace "Tabbed"] $
        tabbed shrinkText myTabConfig
    myTabConfig =
      def
        { activeColor = "#737aa2",
          inactiveColor = "#24283b"
        }
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

-- Xmobar
xmobar1 = statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc_laptop" (pure myXmobarPP)

xmobar2 = statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc_hdmi" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = pink " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#ff79c6" 2,
      ppHidden = cultured . wrap " " "",
      ppHiddenNoWindows = outerspace . wrap " " "",
      ppUrgent = sunsetorange . wrap (keylime "!") (keylime "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (cultured "[") (cultured "]") . pink . ppWindow
    formatUnfocused = wrap (outerspace "[") (outerspace "]") . cultured . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

    outerspace, pink, sunsetorange, cultured, keylime :: String -> String
    pink = xmobarColor "#ff79c6" ""
    cultured = xmobarColor "#f8f8f2" ""
    keylime = xmobarColor "#f1fa8c" ""
    sunsetorange = xmobarColor "#ff5555" ""
    outerspace = xmobarColor "#44475a" ""

-- Autostart
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "mons -e left && ~/.fehbg"
  spawnOnce "xautolock -time 10 -locker slock -detectsleep"
  spawnOnce
    "trayer -l --edge top --align right --SetDockType true \
    \--SetPartialStrut true --expand true --widthtype request \
    \--transparent true --tint 0x232634 --height 18 \
    \--monitor 0"
  spawnOnce "redshift-gtk"
  spawnOnce "udiskie"
  spawnOnce "nm-applet"
  spawnOnce "arch-audit-gtk"
  spawnOnce "calcurse --daemon"
  spawnOnce "dunst"
  spawnOnce "lxpolkit"
  spawnOnce "numlockx"
  spawnOnce "blueman-applet"

-- Manage hook
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gpodder" --> doShift "5:\xead9 ",
      className =? "An Anime Game Launcher" --> doShift "3:\xf1b6 ",
      className =? "Qalculate-gtk" --> doFloat,
      className =? "Signal" --> doShift "4:\xf10b ",
      className =? "Spotify" --> doShift "5:\xead9 ",
      className =? "Transmission-gtk" --> doShift "2:\xe743 ",
      className =? "Viewnior" --> doFloat,
      className =? "Vivaldi-stable" --> doShift "2:\xe743 ",
      className =? "Xmessage" --> doFloat,
      className =? "Xreader" --> doShift "6:\xeb69 ",
      className =? "calibre" --> doShift "6:\xeb69 ",
      className =? "discord" --> doShift "4:\xf10b ",
      className =? "explorer.exe" --> doShift "3:\xf1b6 ",
      className =? "feh" --> doFloat,
      title =? "glxgears" --> doFloat,
      className =? "library_manager" --> doShift "6:\xeb69 ",
      className =? "libreoffice-calc" --> doShift "6:\xeb69 ",
      className =? "libreoffice-writer" --> doShift "6:\xeb69 ",
      className =? "mpv" --> doFullFloat,
      className =? "mpv" --> doShift "5:\xead9 ",
      className =? myTerminal --> doShift "1:\xf489 ",
      title =? "Neverwinter" --> doShift "3:\xf1b6 ",
      className =? "steam" --> doShift "3:\xf1b6 ",
      className =? "steam_app_109600" --> doFloat,
      className =? "transmission-gtk" --> doShift "2:\xe743 ",
      isDialog --> doFloat,
      isFullscreen --> doFullFloat
    ]
    <+> namedScratchpadManageHook myScratchpads

-- Handle event hook
myHandleEventHook :: Event -> X All
myHandleEventHook =
  handleEventHook def
    <> Hacks.trayerAboveXmobarEventHook
    <> Hacks.trayerPaddingXmobarEventHook
    <> Hacks.fixSteamFlicker
