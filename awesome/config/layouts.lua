local awful = require("awful")

local layouts = {}

function layouts.init()
   context.layouts = {
      awful.layout.suit.tile,
      awful.layout.suit.floating,
      awful.layout.suit.tile.left,
      awful.layout.suit.tile.bottom,
      awful.layout.suit.tile.top,
      awful.layout.suit.fair,
      awful.layout.suit.fair.horizontal,
      awful.layout.suit.spiral,
      awful.layout.suit.spiral.dwindle,
      awful.layout.suit.max,
      awful.layout.suit.max.fullscreen,
      awful.layout.suit.magnifier,
      awful.layout.suit.corner.nw,
   }
   awful.layout.layouts = context.layouts

   awful.screen.connect_for_each_screen(function(s)
      awful.tag({ "", "", "", "", "", "", "" }, s, awful.layout.layouts[1])

   end)

end

return layouts
