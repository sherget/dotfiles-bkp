-- IMPORTS --

-- Base
import XMonad
import System.IO
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
-- Data
import Data.List
-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.WithAll
import XMonad.Actions.MouseResize
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
-- Utility
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Replace
import XMonad.Util.SpawnOnce
-- Layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Spiral

-- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BorderResize
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
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
myBorderWidth = 1

-- KEYBINDINGS
myKeys = [ ("M-C-r", spawn "xmonad --recompile")   -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")      -- Restarts xmonad
        , ("M-S-<Esc>", io exitSuccess)            -- Quits xmonad
        , ("M-q", kill1)                           -- Kill the currently focused client
        , ("M-S-q", killAll)                       -- Kill all windows on current workspace
        , ("M-<Return>", spawn myTerminal)
        , ("M-d", spawn "dmenu_run")                       -- Run dmenu
        ]

-- LAYOUTS
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =   tall
                                ||| magnify
                                ||| noBorders monocle
                                ||| floats
                                ||| grid
                                ||| noBorders tabs
                                ||| spirals
                                ||| threeCol
                                ||| threeRow

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
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
    , manageHook         = manageDocks <+> manageHook defaultConfig
    , layoutHook         = myLayoutHook
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor xmobarTitleColor "" . shorten 100,
                          ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                        }
  } `additionalKeysP` myKeys
