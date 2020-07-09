-- IMPORTS --

-- Base
import XMonad
import System.IO
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Data
import Data.List
import Data.Monoid

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.WithAll
import XMonad.Actions.MouseResize

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.FadeWindows

-- Utility
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Replace
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

-- Layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Spiral
import XMonad.Layout.Fullscreen

-- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BorderResize
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.ToggleLayouts

-- VARIABLES --
myTerminal = "st"

xmobarCurrentWorkspaceColor = "#00ccff"
xmobarTitleColor = "#e88915"

myNormalBorderColor = "#777777"
myFocusedBorderColor = "#00ccff"
myBorderWidth = 0
topbarHeight = 5

myFocusFollowsMouse  = False
myClickJustFocuses   = True

-- EVENTHOOK
myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    where
        myDynHook = composeAll
            [
            --  isPersonalHangouts --> forceCenterFloat
            --, isWorkHangouts --> insertPosition End Newer
            ]

-- SCRATCHPADS
scratchpads = [ NS "ranger" "st -c 'ranger' -e ranger" (className =? "ranger") manageTerm
              ,  NS "notes" "st -c 'scratchpad' -e 'nvim'" (className =? "scratchpad") manageTerm
              ,  NS "pavu" "pavucontrol" (className =? "Pavucontrol") manageWindow
              ,  NS "networkmanager" "nm-connection-editor" (className =? "Nm-connection-editor") manageWindow
              ,  NS "trello" "npm start --prefix ~/Applications/trello/" (className =? "Trello") manageTerm
              ]
  where
manageTerm = customFloating $ W.RationalRect l t w h
           where
             h = 0.9
             w = 0.9
             t = 0.95 -h
             l = 0.95 -w

manageWindow = customFloating $ W.RationalRect l t w h
           where
             h = 0.6
             w = 0.6
             t = 0.80 -h
             l = 0.80 -w

-- KEYBINDINGS
myKeys = [ ("M-C-r", spawn "xmonad --recompile")   -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")      -- Restarts xmonad
        , ("M-S-<Esc>", io exitSuccess)            -- Quits xmonad
        , ("M-q", kill1)                           -- Kill the currently focused client
        , ("M-S-q", killAll)                       -- Kill all windows on current workspace
        , ("M-<Return>", spawn myTerminal)
        , ("M-d", spawn "dmenu_run")               -- Run dmenu
        , ("C-x", sendMessage ToggleStruts)      -- Toggle xmobar
        , ("M-n", namedScratchpadAction scratchpads "notes")
        , ("M-e", namedScratchpadAction scratchpads "ranger")
        , ("M-m", namedScratchpadAction scratchpads "networkmanager")
        , ("M-p", namedScratchpadAction scratchpads "pavu")
        , ("M-S-t", namedScratchpadAction scratchpads "trello")
   --     , ("M-S-u", sendMessage $ JumpToLayout "Unflexed")
   --     , ("M-S-i", sendMessage $ JumpToLayout "Monocle")
   --     , ("M-S-o", sendMessage $ JumpToLayout "Tabs")
   --     , ("M-S-p", sendMessage $ JumpToLayout "Tall")
        ]

-- LAYOUTS
myLayoutHook = avoidStruts
              $ mouseResize
              $ windowArrange
              $ T.toggleLayouts threeCol
              $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =   threeCol
                                ||| noBorders monocle
                                ||| noBorders tabs
                                ||| tall
                               -- ||| magnify
                               -- ||| floats
                               -- ||| grid
                               -- ||| spirals
                               -- ||| threeRow

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining Layouts
--magnify  = renamed [Replace "magnify"]
--           $ magnifier
--           $ limitWindows 12
--           $ mySpacing 8
--           $ ResizableTall 1 (3/100) (1/2) []
--floats   = renamed [Replace "floats"]
--           $ limitWindows 20 simplestFloat
--grid     = renamed [Replace "grid"]
--           $ limitWindows 12
--           $ mySpacing 8
--           $ mkToggle (single MIRROR)
--           $ Grid (16/10)
--spirals  = renamed [Replace "spirals"]
--           $ mySpacing 8
--           $ spiral (6/7)
--threeCol = renamed [Replace "threeCol"]
--           $ limitWindows 7
--           $ mySpacing 4
--           $ ThreeCol 1 (3/100) (1/2)
--threeRow = renamed [Replace "threeRow"]
--           $ limitWindows 7
--           $ mySpacing 4
--           -- Mirror takes a layout and rotates it by 90 degrees.
--           -- So we are applying Mirror to the ThreeCol layout.
--           $ Mirror
--           $ ThreeCol 1 (3/100) (1/2)
threeCol = renamed [Replace "Unflexed"]
         $ mySpacing 3
         $ ThreeColMid 1 (1/10) (1/2)

tall     = renamed [Replace "Tall"]
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (1/100) (1/2) []

monocle  = renamed [Replace "Monocle"]
           $ limitWindows 20 Full

tabs     = renamed [Replace "Tabs"]
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

-- Startuphook --
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "nm-applet &"
  spawnOnce "volumeicon &"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 2 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282A36 --height 22 &"
  spawnOnce "setxkbmap -option caps:escape"
  setWMName "LG3D"

-- Workspaces --
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = ["dev", "www", "misc"] ++ map show [4..9]

-- MAIN --
main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/shin/.xmonad/xmobarrc"
  xmonad $ docks defaultConfig {
    modMask              = mod4Mask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , manageHook         = manageDocks <+> namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
    , layoutHook         = myLayoutHook
    , handleEventHook    = myHandleEventHook
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor xmobarTitleColor "" . shorten 80,
                          ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                        }
  } `additionalKeysP` myKeys
