
local utils = require 'mp.utils'
local msg = require 'mp.msg'


local function exec(process)
    p_ret = utils.subprocess({args = process})
    if p_ret.error and p_ret.error == "init" then
        print("ERROR executable not found: " .. process[1])
    end
    return p_ret
end


local bat = exec({"/usr/bin/acpi", "-b"})
if bat.error then
    msg.error("Battery detection failed")
end


function is_desktop()
    return string.find(bat.stdout, "No support")
end

function is_laptop()
    return string.find(bat.stdout, "Battery")
end

function on_battery()
    local bat = exec({"/usr/bin/acpi", "-b"})
    return string.find(bat.stdout, "Discharging")
end
