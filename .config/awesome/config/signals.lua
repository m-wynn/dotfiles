local awful = require("awful")
local beautiful = require("beautiful")

local signals = {}

function signals.init(_)

  client.connect_signal("manage", function (c)

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
    end
  end)


  -- Enable sloppy focus, so that focus follows mouse.
  client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
      and awful.client.focus.filter(c) then
      client.focus = c
    end
  end)

  client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
  client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

end

return signals
