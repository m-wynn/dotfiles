local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local wallpaper = {}

function wallpaper.init()
  local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
    end
  end

  screen.connect_signal("property::geometry", set_wallpaper)

  awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)
  end)

  local function scandir(directory, filter)
   local i, t, popen = 0, {}, io.popen
   if not filter then
      filter = function(s) return true
      end
   end
   print(filter)
   for filename in popen('ls -a "'..directory..'"'):lines() do
      if filter(filename) then
         i = i + 1
         t[i] = filename
      end
   end
   return t
end

  wp_index = 1
  wp_timeout  = 300
  wp_path = os.getenv("HOME") .. "/Pictures/wallpaper/"
  wp_filter = function(s) return string.match(s,"%.png$") or string.match(s,"%.jpg$")
  end
  wp_files = scandir(wp_path, wp_filter)

  -- setup the timer
  wp_timer = timer { timeout = wp_timeout }
  wp_timer:connect_signal("timeout", function()

    -- set wallpaper to current index for all screens
    for s = 1, screen.count() do
      gears.wallpaper.maximized(wp_path .. wp_files[wp_index], s, true)
    end

    -- stop the timer (we don't need multiple instances running at the same time)
    wp_timer:stop()

    -- get next random index
    wp_index = math.random( 1, #wp_files)

    --restart the timer
    wp_timer.timeout = wp_timeout
    wp_timer:start()
  end)

  -- initial start when rc.lua is first run
  wp_timer:start()

end

return wallpaper
