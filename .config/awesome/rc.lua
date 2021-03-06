local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")

terminal = "alacritty" or "urxvt" or "st"
editor = os.getenv("EDITOR") or "vim" or "vi"
editor_cmd = terminal .. " -e " .. editor

context = {

  modkey = "Mod4",
  altkey = "Mod1",

  theme_file = awful.util.getdir("config") .. "/theme/theme.lua",

  cmds = {
    terminal = terminal,
    editor_cmd = editor_cmd,
  },

  widgets = {},
  menu = {},
}

beautiful.init(context.theme_file)

local config = require("config")
config.notify.init(context)
config.layouts.init(context)
config.widgets.init(context)
config.toolbar.init(context)
config.keys.init(context)
config.signals.init(context)
config.rules.init(context)
config.wallpaper.init(context)
