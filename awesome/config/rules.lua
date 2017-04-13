local awful = require("awful")
local beautiful = require("beautiful")

local rules = {}

function rules.init()

  awful.rules.rules = {
    {
      rule = {},
      properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        keys = clientkeys,
        buttons = clientbuttons,
        screen = awful.screen.preferred,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen
      }
    },

    -- Floating clients.
    {
      rule_any = {
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"
        },

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }
    },

    -- Don't add titlebars to normal clients and dialogs
    {
      rule_any = {
        type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    {
      rule = { class = "Firefox" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { class = "discord" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { class = "HipChat" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { class = "Nemo" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { instance = "ncmpcpp" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { class = "Pavucontrol" },
      properties = { screen = 1, tag = "" }
    },
    {
      rule = { instance = "mutt" },
      properties = { screen = 1, tag = "" }
    },
  }

end

return rules
