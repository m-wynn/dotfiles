local wibox = require("wibox")
local beautiful = require("beautiful")
local awful = require("awful")
local lain = require("lain")

local toolbar = {}

function toolbar.init(awesome_context)
  local widgets = awesome_context.widgets

  -- Separators
  separators = lain.util.separators
  spr = wibox.widget.textbox(' ')
  arrl = wibox.widget.imagebox()
  arrl:set_image(beautiful.arrl)
  arrl_dl = separators.arrow_left(beautiful.bg_focus, "alpha")
  arrl_ld = separators.arrow_left("alpha", beautiful.bg_focus)

  awful.screen.connect_for_each_screen(function(s)
    local si = s.index

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Left-aligned widgets
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(widgets.screen[si].mytaglist)
    left_layout:add(widgets.screen[si].mypromptbox)

    -- Right-aligned widgets
    local right_layout = wibox.layout.fixed.horizontal()

    local right_layout_toggle = true
    local function right_layout_add (...)
      local arg = {...}
      if right_layout_toggle then
        right_layout:add(arrl_ld)
        for i, n in pairs(arg) do
          right_layout:add(wibox.container.background(n ,beautiful.bg_focus))
        end
      else
        right_layout:add(arrl_dl)
        for i, n in pairs(arg) do
          right_layout:add(n)
        end
      end
      right_layout_toggle = not right_layout_toggle
    end

    right_layout:add(wibox.widget.systray())
    right_layout_add(widgets.mpdicon, widgets.mpdwidget)
    right_layout_add(widgets.volumeicon, widgets.volumewidget)
    right_layout:add(widgets.mytextclock)
    right_layout:add(widgets.screen[si].mylayoutbox)

    -- Bring it all together with the tags in the middle
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(widgets.screen[si].mytasklist)
    layout:set_right(right_layout)

    s.mywibox:set_widget(layout)
  end)


end

return toolbar
