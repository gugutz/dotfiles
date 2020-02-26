-- Imports.
import XMonad
import XMonad.Hooks.DynamicLog

-- Main configuration, override the defaults to your liking.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- another way to start the main funciton, copied from the arch wiki
-- main = do
--   xmonad $ defaultConfig
--     { terminal    = myTerminal
--     , modMask     = myModMask
--     , borderWidth = myBorderWidth
--     }
-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


myConfig = defaultConfig {
modMask = mod4Mask,  -- Win key or Super_L
terminal    = "urxvt",
borderWidth = 3
}

