local run_once = require("helpers").run_once

local autorun = {}

function autorun.init(awesome_context)
  run_once("compton --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc --unredir-if-possible --no-fading-openclose &")
  run_once("xrdb ~/.Xresources &")
  run_once("xmodmap ~/.Xmodmap &")
  run_once("urxvtd &")
  run_once("unclutter -root &")
  run_once("mpd &")
end

return autorun
