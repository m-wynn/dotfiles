local wezterm = require 'wezterm'
local act = wezterm.action

local is_linux = wezterm.target_triple == "x86_64-unknown-linux-gnu"
local is_windows = wezterm.target_triple == "x86_64-pc-windows-msvc"
local is_mac = wezterm.target_triple == "aarch64-apple-darwin"

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = "Catppuccin Mocha"
config.default_domain = is_windows and "WSL:fedora" or nil
config.default_prog = is_windows and { "wsl.exe" } or nil
config.enable_tab_bar = false
config.font = wezterm.font 'Fira Code'
config.keys = {
  { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
}
return config
