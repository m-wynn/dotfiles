local awful = require("awful")
local util = require("awful.util")
local lain = require("lain")
local beautiful = require("beautiful")
local wibox = require("wibox")
local helpers = require("helpers")
local vicious = require("vicious")

local widget_loader = {}

function widget_loader.init(awesome_context)
  local w = awesome_context.widgets
  local markup = lain.util.markup

  -- Clock
  w.mytextclock = wibox.widget.textclock()

  -- volume
  w.volumeicon = wibox.widget.imagebox(beautiful.widget_vol)
  w.volumewidget = lain.widget.alsa(
    {
      settings = function()
        if volume_now.status == "off" then
          w.volumeicon:set_image(beautiful.widget_vol_mute)
        elseif tonumber(volume_now.level) == 0 then
          w.volumeicon:set_image(beautiful.widget_vol_no)
        elseif tonumber(volume_now.level) <= 30 then
          w.volumeicon:set_image(beautiful.widget_vol_low)
        else
          w.volumeicon:set_image(beautiful.widget_vol)
        end

        widget:set_text(" " .. volume_now.level .. "% ")
      end
    }
  )

  -- music
  w.musicicon = wibox.widget.imagebox(beautiful.widget_music)
  w.musicwidget = awful.widget.watch({"pidof", "Google Play Music Desktop Player"}, 2, function(widget, stdout)
    local file_location = os.getenv("HOME") .. "/.config/Google Play Music Desktop Player/json_store/playback.json"
    local filelines = helpers.get_lines(file_location)
    if not filelines then
      widget:set_text(" not running ")
      return
    end -- GPMDP not running?

    gpm_now = { running = stdout ~= '' }

    if not next(filelines) then
      gpm_now.running = false
      gpm_now.playing = false
    else
      dict, pos, err    = require("lain.util").dkjson.decode(table.concat(filelines), 1, nil) -- lain
      gpm_now.artist    = dict.song.artist
      gpm_now.album     = dict.song.album
      gpm_now.title     = dict.song.title
      gpm_now.cover_url = dict.song.albumArt
      gpm_now.playing   = dict.playing
    end

    if gpm_now.playing then
      artist = " " .. helpers.sanitize_markup(helpers.utf8sub(gpm_now.artist, 1, 20)) .. " "
      title  = helpers.sanitize_markup(helpers.utf8sub(gpm_now.title, 1, 20)) .. " "
      w.musicicon:set_image(beautiful.widget_music_on)
    elseif gpm_now.running then
      artist = " Music "
      title  = "Paused "
    else
      artist = ""
      title  = ""
      w.musicicon:set_image(beautiful.widget_music)
    end
    widget:set_markup(markup(beautiful.bg_red, artist) .. title)
  end)

  -- RAM
  local innermemwidget = wibox.widget {
    border_color     = nil,
    background_color = beautiful.bg_focus,
    color            = beautiful.fg_urgent,
    widget           = wibox.widget.progressbar,
  }
  w.memwidget = wibox.widget {
    {
      widget      = innermemwidget,
    },
    forced_height = 18,
    forced_width  = 8,
    direction     = "east",
    layout        = wibox.container.rotate,
  }
  vicious.cache(vicious.widgets.mem)
  vicious.register(innermemwidget, vicious.widgets.mem, "$1", 13)
  w.memicon = wibox.widget.imagebox(beautiful.widget_mem)

  -- CPU
  w.cpuicon = wibox.widget.imagebox(beautiful.widget_cpu)
  w.cpuwidget = wibox.widget{
    forced_width = 50,
    forced_height = 18,
    background_color = beautiful.bg_normal,
    color = ({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, beautiful.fg_red}, {0.5, beautiful.fg_green}, {1, beautiful.bg_green}}}),
    widget = wibox.widget.graph
  }
  vicious.cache(vicious.widgets.cpu)
  vicious.register(w.cpuwidget, vicious.widgets.cpu, "$1")

  -- Battery
  w.baticon = wibox.widget.imagebox(beautiful.widget_battery)
  w.batwidget = lain.widget.bat(
    {
      battery = "BAT0",
      settings = function()
        if bat_now.perc == "N/A" then
          widget:set_markup(" AC ")
          w.baticon:set_image(beautiful.widget_ac)
          return
        elseif tonumber(bat_now.perc) <= 5 then
          w.baticon:set_image(beautiful.widget_battery_empty)
        elseif tonumber(bat_now.perc) <= 15 then
          w.baticon:set_image(beautiful.widget_battery_low)
        else
          w.baticon:set_image(beautiful.widget_battery)
        end
        widget:set_markup(" " .. bat_now.perc .. "% ")
      end
    })

  -- Net
  w.neticon = wibox.widget.imagebox(beautiful.widget_net)
  w.netwidget = lain.widget.net(
    {
      settings = function()
        widget:set_markup(markup(beautiful.bg_green, " " .. net_now.received)
                          .. " " ..
                          markup(beautiful.bg_blue, " " .. net_now.sent .. " "))
      end
    })

  -- taglist
  w.taglist_buttons = awful.util.table.join(
    awful.button({}, 1,
                 function(t) t:view_only() end
                 ),
    awful.button({ modkey }, 1,
                 function(t)
                   if client.focus then
                     client.focus:move_to_tag(t)
                   end
                 end
                 ),
    awful.button({}, 3,
                 awful.tag.viewtoggle
                 ),
    awful.button({ modkey }, 3,
                 function(t)
                   if client.focus then
                     client.focus:toggle_tag(t)
                   end
                 end
                 ),
    awful.button({}, 4,
                 function(t) awful.tag.viewnext(t.screen) end
                 ),
    awful.button({}, 5,
                 function(t) awful.tag.viewprev(t.screen) end
               )
  )

  w.screen = {}
  awful.screen.connect_for_each_screen(function(s)
    local si = s.index
    w.screen[si] = {}
    local sw = w.screen[si]

    -- Tasklist
    sw.tasklist_buttons = awful.util.table.join(
      awful.button({}, 1,
                   function (c)
                     if c == client.focus then
                       c.minimized = true
                     else
                       c.minimized = false
                       if not c:isvisible() and c.first_tag then
                         c.first_tag:view_only()
                       end
                       client.focus = c
                       c:raise()
                     end
                   end
                   ),
      awful.button({}, 4,
                   function () awful.client.focus.byidx(1) end
                   ),
      awful.button({}, 5,
                   function () awful.client.focus.byidx(-1) end
                 )
    )


    sw.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, w.taglist_buttons)


    sw.mypromptbox = awful.widget.prompt()

    -- Create a tasklist widget
    sw.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, sw.tasklist_buttons)

    sw.mylayoutbox = awful.widget.layoutbox(s)
    sw.mylayoutbox:buttons(awful.util.table.join(
      awful.button({}, 1, function () awful.layout.inc( 1) end),
      awful.button({}, 3, function () awful.layout.inc(-1) end),
      awful.button({}, 4, function () awful.layout.inc( 1) end),
      awful.button({}, 5, function () awful.layout.inc(-1) end)))


  end)

  return awesome_context
end

return widget_loader
