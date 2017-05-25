-- Helper Functions!

local awful = require("awful")

local helpers = {}

-- Trim UTF-8 strings
-- Credit to jnwhiteh
-- http://wowprogramming.com/snippets/UTF-8_aware_stringsub_7 of all places

local function chsize(char)
  if not char then
    return 0
  elseif char > 240 then
    return 4
  elseif char > 225 then
    return 3
  elseif char > 192 then
    return 2
  else
    return 1
  end
end

function helpers.utf8sub(str, startChar, numChars)
  local startIndex = 1
  while startChar > 1 do
    local char = string.byte(str, startIndex)
    startIndex = startIndex + chsize(char)
    startChar = startChar - 1
  end

  local currentIndex = startIndex

  while numChars > 0 and currentIndex <= #str do
    local char = string.byte(str, currentIndex)
    currentIndex = currentIndex + chsize(char)
    numChars = numChars -1
  end
  return str:sub(startIndex, currentIndex - 1)
end

-- run once
-- runs a command unless it's already running

function helpers.run_once(cmd)
  local findme = cmd
  local firstspace = cmd:find(" &")
  if firstspace then
    findme = cmd:sub(0, firstspace-1)
  end
  awful.spawn.with_shell("pgrep -u $USER -f " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

return helpers
