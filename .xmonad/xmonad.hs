import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.CopyWindow
import XMonad.Hooks.Minimize
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce


import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D


myStartupHook = do
    spawn "$HOME/.config/polybar/launch.sh"
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

-- colours
normBord = "#4c566a"
focdBord = "#5e81ac"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask

myBaseConfig = desktopConfig

myDefaults = def {
          normalBorderColor   = "#1d1f21"
        , focusedBorderColor  = "#288AD6"
        , focusFollowsMouse   = True
        , mouseBindings       = myMouseBindings
        , workspaces          = myWorkspaces
        , keys                = myKeys
        , modMask             = myModMask
        , borderWidth         = 2
        , layoutHook          = myLayoutHook
        , startupHook         = myStartupHook
        , manageHook          = manageSpawn <+> myManageHook <+> manageHook myBaseConfig
        }

encodeCChar = map fromIntegral . B.unpack
myFocusFollowsMouse = True
myBorderWidth = 2
myWorkspaces    = ["\61612","\61899","\61557","\61888","\61664","\61501","\61447","\62003","\61441", "\61723"]
--myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]

terminalScratchpadCmd = "alacritty --title=scratchpad " 
musicScratchpadCmd = "alacritty --title=music --command=ncmpcpp" 
webcamScratchpadCmd = "mpv /dev/video2"

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [
                  NS "terminal" terminalScratchpadCmd (title =? "scratchpad") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
                  NS "music" musicScratchpadCmd (title =? "music") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
                  NS "webcam" webcamScratchpadCmd (title =? "webcam") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                ]


-- window manipulations
myManageHook = composeAll
    [
      className =? "MPlayer"                                 --> doFloat
    , className =? "Gimp"                                    --> doFloat
    , className =? "Nextcloud"                               --> doFloat
    , className =? "Evolution-alarm-notify"                  --> doFloat
    , className =? "TeamViewer"                              --> doFloat
    , resource  =? "desktop_window"                          --> doIgnore
    , resource  =? "kdesktop"                                --> doIgnore
    , className =? "qutebrowser"                             --> doShift ( myWorkspaces !! 0 )
    , className =? "Google-chrome"                           --> doShift ( myWorkspaces !! 0 )
    , className =? "Brave-browser"                           --> doShift ( myWorkspaces !! 0 )
    , className =? "firefox"                                 --> doShift ( myWorkspaces !! 0 )
    , className =? "calibre"                                 --> doShift ( myWorkspaces !! 0 )
    , className =? "LibreOffice"                             --> doShift ( myWorkspaces !! 0 )
    , className =? "libreoffice-calc"                        --> doShift ( myWorkspaces !! 0 )
    , className =? "Emacs"                                   --> doShift ( myWorkspaces !! 1 )
    , className =? "jetbrains-phpstorm"                      --> doShift ( myWorkspaces !! 1 )
    , className =? "Slack"                                   --> doShift ( myWorkspaces !! 2 )
    , className =? "TelegramDesktop"                         --> doShift ( myWorkspaces !! 2 )
    , className =? "whatsdesk"                               --> doShift ( myWorkspaces !! 2 )
    , className =? "Signal"                                  --> doShift ( myWorkspaces !! 2 )
    , className =? "Skype"                                   --> doShift ( myWorkspaces !! 2 )
    , className =? "Microsoft Teams - Preview"               --> doShift ( myWorkspaces !! 2 )
    , className =? "DBeaver"                                 --> doShift ( myWorkspaces !! 3 )
    , className =? "Insomnia Designer"                       --> doShift ( myWorkspaces !! 4 )
    , className =? "Insomnia"                                --> doShift ( myWorkspaces !! 4 )
    , className =? "Stoplight Studio"                        --> doShift ( myWorkspaces !! 4 )
    , className =? "Postman"                                 --> doShift ( myWorkspaces !! 4 )
    , className =? "zoom"                                    --> doShift ( myWorkspaces !! 5 )
    , className =? "Jitsi Meet"                              --> doShift ( myWorkspaces !! 5 )
    , className =? "obs"                                     --> doShift ( myWorkspaces !! 5 )
    , className =? "VirtualBox Manager"                      --> doShift ( myWorkspaces !! 7 )
    , className =? "VirtualBox Machine"                      --> doShift ( myWorkspaces !! 7 )
    , className =? "Virt-manager"                            --> doShift ( myWorkspaces !! 7 )
    , className =? "Spotify"                                 --> doShift ( myWorkspaces !! 8 )
    , className =? "Mixxx"                                   --> doShift ( myWorkspaces !! 8 )
    , className =? "Nextcloud"                               --> doShift ( myWorkspaces !! 9 )
    , className =? "Steam"                                   --> doShift ( myWorkspaces !! 9 )
    , isDialog --> doCenterFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ] <+> namedScratchpadManageHook myScratchPads
-- keys config


myLayoutHook =
  spacingRaw True (Border 0 0 0 0) True (Border 20 20 20 20) True $
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
  smartBorders $
  avoidStruts $
  gaps [(U,35), (D,35), (R,35), (L,35)] $
  (tiled  ||| (ThreeColMid 1 (1/100) (1/2)) ||| Full)
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    ]


-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [
  -- MODKEY + ...
    ((modMask, xK_r),       withFocused $ windows . W.sink)
  , ((modMask, xK_t),       namedScratchpadAction myScratchPads "terminal")
  , ((modMask, xK_f),       sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
  , ((modMask, xK_q),       kill)
  , ((modMask, xK_g),       sendMessage $ ToggleGaps)


  -- MODKEY + SHIFT KEYS
  , ((modMask .|. shiftMask, xK_b),          spawn $ "polybar-msg cmd toggle" )
  , ((modMask .|. shiftMask, xK_v),          namedScratchpadAction myScratchPads "vpn")
  , ((modMask .|. shiftMask, xK_r),          spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask, xK_m),          namedScratchpadAction myScratchPads "music")
  , ((modMask .|. shiftMask, xK_w),          namedScratchpadAction myScratchPads "webcam")
  , ((modMask .|. shiftMask, xK_Return ),    spawn $ "nautilus")
  , ((modMask .|. shiftMask, xK_e ),         spawn $ "~/.config/rofi/power-menu.sh")
 

  -- FUNCTION KEYS
  , ((0, xK_F7), spawn $ "flameshot gui")
  , ((0, xK_F11), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
  , ((0, xF86XK_Search), spawn $ "~/.config/rofi/launcher.sh")

  -- MODKEY + FUNCTIONS KEYS
  , ((modMask, xK_F7), spawn $ "flameshot screen --path ~/Downloads")

  -- MODKEY + ALT + FUNCTIONS KEYS
  , ((modMask .|. mod1Mask, xK_F7), spawn $ "flameshot screen --clipboard --path ~/Downloads")

  -- SUPER + ALT KEYS for Launching Apps
  , ((modMask .|. mod1Mask, xK_a),       spawn $ "emacsclient -c -a '' --eval '(itechytony/day-view)'")
  , ((modMask .|. mod1Mask, xK_e),       spawn $ "emacs")
  , ((modMask .|. mod1Mask, xK_m),       spawn $ "xfce4-settings-manager")
  , ((modMask .|. mod1Mask, xK_r),       setLayout $ XMonad.layoutHook conf)
  , ((modMask .|. mod1Mask, xK_s),       spawn $ "slack")
  , ((modMask .|. mod1Mask, xK_t),       spawn $ "teams")
  , ((modMask .|. mod1Mask, xK_p),       spawn $ "pamac-manager")
  , ((modMask .|. mod1Mask, xK_v),       spawn $ "pavucontrol")
  , ((modMask .|. mod1Mask, xK_f),       spawn $ "firefox")
  , ((modMask .|. mod1Mask, xK_g),       spawn $ "google-chrome-stable -no-default-browser-check")
  , ((modMask .|. mod1Mask, xK_b),       spawn $ "brave")
  , ((modMask .|. mod1Mask, xK_q),       spawn $ "qutebrowser")
  , ((modMask .|. mod1Mask, xK_w),       spawn $ "whatsapp-for-linux")
  , ((modMask .|. mod1Mask, xK_d),       spawn $ "dbeaver")
  , ((modMask .|. mod1Mask, xK_z),       spawn $ "zoom")
  , ((modMask .|. mod1Mask, xK_i),       spawn $ "insomnia")
  , ((modMask .|. mod1Mask, xK_c),       spawn $ "rofi -show calc")
  , ((modMask .|. mod1Mask, xK_o),       spawn $ "picom-toggle")
  , ((modMask .|. mod1Mask, xK_j),       spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((modMask .|. mod1Mask, xK_k),       spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + ALT KEYS
  , ((controlMask .|. mod1Mask, xK_Delete ), spawn $ "xfce4-taskmanager")

  --SCREENSHOTS
  , ((0, xK_Print), spawn $ "maim -s | xclip -selection clipboard -t image/png")
  , ((modMask, xK_Print), spawn $ "maim ~/Downloads/screenshot_$(date +%s).png")
  , ((modMask .|. mod1Mask, xK_Print), spawn $ "flameshot screen --clipboard --path ~/Downloads")

  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "pamixer --toggle-mute")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "pamixer -d 5")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "pamixer -i 5")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "light -A 1")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "light -U 1")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_Tab), toggleWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask, xK_grave), sendMessage NextLayout)

  --Focus selected desktop
  , ((modMask , xK_bracketleft ), prevWS)

  --Focus selected desktop
  , ((modMask , xK_bracketright ), nextWS)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)
  , ((modMask, xK_Down), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp )
  , ((modMask, xK_Up), windows W.focusUp )


  , ((modMask, xK_s ), windows copyToAll) -- @@ Make focused window always visible
  , ((modMask .|. shiftMask, xK_s ),  killAllOtherCopies) -- @@ Toggle window state back


  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
  , ((modMask .|. shiftMask, xK_Up), windows W.swapUp)

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
  , ((modMask .|. shiftMask, xK_Down), windows W.swapDown)

  -- Shrink the master area.
  , ((modMask, xK_l), sendMessage Expand)
  , ((modMask, xK_Right), sendMessage Expand)

  -- Expand the master area.
  , ((modMask, xK_h), sendMessage Shrink)
  , ((modMask, xK_Left), sendMessage Shrink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_equal), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_minus), sendMessage (IncMasterN (-1)))

  -- Move focus to the master window.
  -- , ((modMask, xK_0), setLayout $ XMonad.layoutHook conf )

  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

  --French Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla , xK_agrave]

  --Belgian Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_section, xK_egrave, xK_exclam, xK_ccedilla, xK_agrave]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]

  -- ++
  -- -- ctrl-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --     | (key, sc) <- zip [xK_w, xK_e] [0..]
  --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad . ewmh $ myDefaults
