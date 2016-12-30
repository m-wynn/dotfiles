local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local menubar = require("menubar")

local keys = {}
function keys.init(awesome_context)

  local modkey = awesome_context.modkey
  local altkey = awesome_context.altkey
  local cmd = awesome_context.cmds

  local bind_key = function(mod, key, press, description, group)
    return awful.key.new(mod, key, press, nil, {description=description, group=group})
  end


  local globalkeys = awful.util.table.join(

    -- Help
    bind_key({ modkey }, "s",
             hotkeys_popup.show_help,
             "show help", "awesome"
             ),

    -- Standard program
    bind_key({ modkey }, "Return",
             function () awful.spawn(terminal) end,
             "open a terminal", "launcher"
             ),
    bind_key({ modkey, "Control" }, "r",
             awesome.restart,
             "reload awesome", "awesome"
             ),
    bind_key({ modkey, "Shift"   }, "q",
             awesome.quit,
             "quit awesome", "awesome"
             ),

    -- Navigation and selection
    bind_key({ modkey }, "Left",
             awful.tag.viewprev,
             "view previous", "tag"
             ),
    bind_key({ modkey }, "Right",
             awful.tag.viewnext,
             "view next", "tag"
             ),
    bind_key({ modkey }, "Escape",
             awful.tag.history.restore,
             "go back", "tag"
             ),
    bind_key({ modkey }, "j",
             function () awful.client.focus.byidx( 1) end,
             "focus next by index", "client"
             ),
    bind_key({ modkey }, "k",
             function () awful.client.focus.byidx(-1) end,
             "focus previous by index", "client"
             ),

    -- Layout manipulation
    bind_key({ modkey, "Shift" }, "j",
             function () awful.client.swap.byidx(  1) end,
             "swap with next client by index", "client"
             ),
    bind_key({ modkey, "Shift" }, "k",
             function () awful.client.swap.byidx( -1) end,
             "swap with previous client by index", "client"
             ),
    bind_key({ modkey, "Control" }, "j",
             function () awful.screen.focus_relative( 1) end,
             "focus the next screen", "screen"
             ),
    bind_key({ modkey, "Control" }, "k",
             function () awful.screen.focus_relative(-1) end,
             "focus the previous screen", "screen"
             ),
    bind_key({ modkey }, "u",
             awful.client.urgent.jumpto,
             "jump to urgent client", "client"
             ),
    bind_key({ modkey }, "Tab",
             function ()
               awful.client.focus.history.previous()
               if client.focus then
                 client.focus:raise()
               end
             end,
             "go back", "client"
             ),

    -- Advanced layout manipulation
    bind_key({ modkey }, "l",
             function () awful.tag.incmwfact( 0.05) end,
             "increase master width factor", "layout"
             ),
    bind_key({ modkey }, "h",
             function () awful.tag.incmwfact(-0.05) end,
             "decrease master width factor", "layout"
             ),
    bind_key({ modkey, "Shift"   }, "h",
             function () awful.tag.incnmaster( 1, nil, true) end,
             "increase the number of master clients", "layout"
             ),
    bind_key({ modkey, "Shift"   }, "l",
             function () awful.tag.incnmaster(-1, nil, true) end,
             "decrease the number of master clients", "layout"
             ),
    bind_key({ modkey, "Control" }, "h",
             function () awful.tag.incncol( 1, nil, true) end,
             "increase the number of columns", "layout"
             ),
    bind_key({ modkey, "Control" }, "l",
             function () awful.tag.incncol(-1, nil, true) end,
             "decrease the number of columns", "layout"
             ),
    bind_key({ modkey }, "space",
             function () awful.layout.inc( 1) end,
             "select next", "layout"
             ),
    bind_key({ modkey, "Shift"   }, "space",
             function () awful.layout.inc(-1) end,
             "select previous", "layout"
             ),
    bind_key({ modkey, "Control" }, "n",
             function ()
               local c = awful.client.restore()
               -- Focus restored client
               if c then
                 client.focus = c
                 c:raise()
               end
             end,
             "restore minimized", "client"
             ),

    -- Prompt
    bind_key({ modkey }, "r",
             function () awesome_context.widgets.screen[awful.screen.focused().index].mypromptbox:run() end,
             "run prompt", "launcher"
             ),
    bind_key({ modkey }, "x",
             function ()
               awful.prompt.run {
                 prompt       = "Run Lua code: ",
                 textbox      = awesome_context.widgets.screen[awful.screen.focused().index].mypromptbox.widget,
                 exe_callback = awful.util.eval,
                 history_path = awful.util.get_cache_dir() .. "/history_eval"
               }
             end,
             "lua execute prompt", "awesome"
             ),

    -- Menubar
    bind_key({ modkey }, "p",
             function() menubar.show() end,
             "show the menubar", "launcher"
             ),

    -- ALSA volume control
    awful.key({}, "XF86AudioRaiseVolume",
              function()
                os.execute(string.format("amixer -q set %s 1%%+", awesome_context.widgets.volumewidget.channel))
                awesome_context.widgets.volumewidget.update()
              end
              ),
    awful.key({}, "XF86AudioLowerVolume",
              function()
                os.execute(string.format("amixer -q set %s 1%%-", awesome_context.widgets.volumewidget.channel))
                awesome_context.widgets.volumewidget.update()
              end
              ),
    awful.key({}, "XF86AudioMute",
              function()
                os.execute(string.format("amixer -q set %s playback toggle", awesome_context.widgets.volumewidget.channel))
                awesome_context.widgets.volumewidget.update()
              end
              ),

    -- MPD control
    awful.key({}, "XF86AudioPlay",
              function()
                awful.spawn.with_shell("mpc toggle || ncmpcpp toggle || ncmpc toggle || pms toggle")
                awesome_context.widgets.mpdwidget.update()
              end
              ),
    awful.key({}, "XF86AudioPrev",
              function()
                awful.spawn.with_shell("mpc prev || ncmpcpp prev || ncmpc prev || pms prev")
                awesome_context.widgets.mpdwidget.update()
              end
              ),
    awful.key({}, "XF86AudioNext",
              function()
                awful.spawn.with_shell("mpc next || ncmpcpp next || ncmpc next || pms next")
                awesome_context.widgets.mpdwidget.update()
              end
              ),

    -- Brightness control
    awful.key({}, "XF86MonBrightnessDown",
              function () awful.util.spawn("xbacklight -dec 10") end
              ),
    awful.key({}, "XF86MonBrightnessUp",
              function () awful.util.spawn("xbacklight -inc 10") end
              ),

    -- Lockscreen
    awful.key({ altkey, "Control" }, "l",
              function () awful.util.spawn(os.getenv("HOME") .. "/bin/i3blur.sh &") end
            )
  )


  clientkeys = awful.util.table.join(
    bind_key({ modkey }, "f",
             function (c)
               c.fullscreen = not c.fullscreen
               c:raise()
             end,
             "toggle fullscreen", "client"
             ),
    bind_key({ modkey, "Shift"   }, "c",
             function (c) c:kill() end,
             "close", "client"
             ),
    bind_key({ modkey, "Control" }, "space",
             awful.client.floating.toggle,
             "toggle floating", "client"
             ),
    bind_key({ modkey, "Control" }, "Return",
             function (c) c:swap(awful.client.getmaster()) end,
             "move to master", "client"
             ),
    bind_key({ modkey }, "o",
             function (c) c:move_to_screen() end,
             "move to screen", "client"
             ),
    bind_key({ modkey }, "t",
             function (c) c.ontop = not c.ontop end,
             "toggle keep on top", "client"
             ),
    bind_key({ modkey }, "n",
             -- The client currently has the input focus, so it cannot be
             -- minimized, since minimized clients can't have the focus.
             function (c) c.minimized = true end,
             "minimize", "client"
             ),
    bind_key({ modkey }, "m",
             function (c)
               c.maximized = not c.maximized
               c:raise()
             end,
             "maximize", "client"
           )
  )

  -- Bind all key numbers to tags.
  for i = 1, 9 do
    globalkeys = awful.util.table.join(
      globalkeys,
      -- View tag only.
      bind_key({ modkey }, "#" .. i + 9,
               function ()
                 local screen = awful.screen.focused()
                 local tag = screen.tags[i]
                 if tag then
                   tag:view_only()
                 end
               end,
               "view tag #"..i, "tag"
               ),
      -- Toggle tag display.
      bind_key({ modkey, "Control" }, "#" .. i + 9,
               function ()
                 local screen = awful.screen.focused()
                 local tag = screen.tags[i]
                 if tag then
                   awful.tag.viewtoggle(tag)
                 end
               end,
               "toggle tag #" .. i, "tag"
               ),
      -- Move client to tag.
      bind_key({ modkey, "Shift" }, "#" .. i + 9,
               function ()
                 if client.focus then
                   local tag = client.focus.screen.tags[i]
                   if tag then
                     client.focus:move_to_tag(tag)
                   end
                 end
               end,
               "move focused client to tag #"..i, "tag"
               ),
      -- Toggle tag on focused client.
      bind_key({ modkey, "Control", "Shift" }, "#" .. i + 9,
               function ()
                 if client.focus then
                   local tag = client.focus.screen.tags[i]
                   if tag then
                     client.focus:toggle_tag(tag)
                   end
                 end
               end,
               "toggle focused client on tag #" .. i, "tag"
             )
    )
  end

  clientbuttons = awful.util.table.join(
    awful.button({}, 1,
                 function (c) client.focus = c; c:raise() end
                 ),
    awful.button({ modkey }, 1,
                 awful.mouse.client.move
                 ),
    awful.button({ modkey }, 3,
                 awful.mouse.client.resize))

  root.keys(globalkeys)

  root.buttons(
    awful.util.table.join(
      awful.button({}, 4, awful.tag.viewnext),
      awful.button({}, 5, awful.tag.viewprev)
    )
  )

end

return keys
