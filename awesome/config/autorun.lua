local run_once = require("helpers").run_once

local autorun = {}

function autorun.init(awesome_context)
  run_once("compton --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc --unredir-if-possible --no-fading-openclose &")
  run_once("unclutter -root &")
  run_once("urxvt -name mutt -e tmux new-session -s mutt -- mutt")
  run_once("urxvt -name ncmpcpp -e tmux new-session -s ncmpcpp -- ncmpcpp")
end

return autorun
