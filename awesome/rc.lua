-- {{{ Required libraries
local awful      = require("awful")
awful.rules      = require("awful.rules")
                   require("awful.autofocus")
local beautiful  = require("beautiful")
local naughty    = require("naughty")
local tyrannical = require("tyrannical")
-- }}}

-- {{{ Error handling
-- Startup errorsc
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Runtime errors
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true
        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Autostart applications
function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

-- Compositor
run_once("compton -b --detect-rounded-corners --config " .. os.getenv("HOME") .. "/.config/awesome/compton.conf")

-- uncluter
run_once("unclutter")
run_once("killall mission-control-5")
run_once("killall xembedproxy")
run_once("killall xembedsniproxy")
run_once("qdbus org.kde.kactivitymanagerd /ActivityManager org.kde.ActivityManager.Stop")
-- }}}

-- beautiful init
beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/kdesome/theme.lua")

-- common
home       = os.getenv("HOME")
config_dir = home .. "/.config/awesome"
theme_dir  = config_dir .. "/themes/kdesome"
modkey     = "Mod4"
altkey     = "Mod1"

-- user defined
icons       = home .. "/.kde/share/icons"
kdeconf     = "systemsettings5"
-- }}}

-- {{{ Layouts and Tags
-- Available layouts
local layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
}

tyrannical.tags = {
    {
        name        = "#",
        init        = true,
        exclusive   = false,
        screen      = {1,2},
        layout      = awful.layout.suit.max,
        class       = {
          "Atom", "vivaldi-snapshot"
        }
    } ,
    {
        name        = "@",
        init        = true,
        exclusive   = false,
        screen      = 2,
        layout      = awful.layout.suit.tile,
        class = {
          "skypeforlinux", "Thunderbird", "HyperTerm"
        }
    }
}

tyrannical.properties.floating = {
  "yakyak"
}

-- Force the matching clients (by classes) to be centered on the screen on init
tyrannical.properties.centered = {
  "yakyak"
}

-- Clients
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}

-- {{{ Key bindings
-- {{ Global keys

ror = {
  ["h"]={"yakyak", "yakyak" },
  ["a"]={"atom-beta","Atom"},
  ["d"]={"slack", "slack" },
  ["t"]={"thunderbird", "Thunderbird" },
  ["s"]={"skypeforlinux", "skypeforlinux"},
  ["n"]={"dolphin", "Dolphin"},
  ["v"]={"vivaldi-snapshot", "vivaldi-snapshot"},
  ["g"]={"telegram", "telegram"},
  ["k"]={"hyperterm", "HyperTerm"},
  ["w"]={"shodan", "Shodan UI"},
}

globalkeys = awful.util.table.join(
    -- Assistance
    awful.key({ modkey }, ",", function () awful.util.spawn(kdeconf) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),

    -- Forward
    awful.key({ altkey,         }, "Tab",
        function ()
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow')
            local all_clients = client.get()
            for i, c in pairs(all_clients) do
              if c.instance == 'Shadow' then
                client.focus = c
              end
            end
        end),
    awful.key({ altkey,         }, "F2",
        function ()
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode runner')
        end),
    awful.key({ "Control" }, "space",
        function ()
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode ultra')
        end),
    awful.key({ altkey }, "q",
        function (c)
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode time')
        end),

    -- Layout management (tile mode)

    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end)
)

function genfun(t3)
   local cmd=t3[1]
   local rule=t3[2]
   local flag=t3[3]
   local table1={}
   s1="class"
   if flag then
     s1=flag
   end
   table1[s1]=rule
   return function()
      local matcher = function (c)
        return awful.rules.match(c, {class = rule})
      end
      awful.client.run_or_raise(cmd, matcher)
   end
end
function genkeys(mod1)
  rorkeys = awful.util.table.join()
  for i,v in pairs(ror) do
    modifier=""
    if i:len() > 1 then
      modifier=i:sub(1, i:find("-")-1)
      i=i:sub(-1,-1)
    end
    rorkeys = awful.util.table.join(rorkeys,
      awful.key({ mod1, modifier}, i, genfun(v)))
  end
  return rorkeys
end
globalkeys = awful.util.table.join(globalkeys, genkeys(altkey))

-- {{ Client Keys
clientkeys = awful.util.table.join(
    awful.key({ modkey }, "n",
        function (c)
            c.minimized = true
        end),
    awful.key({ modkey }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end),
   awful.key({ modkey }, "c",
        function (c)
            c:geometry({ x = 200,
                         y = 100,
                         width = 1304,
                         height = 688 })
        end),
    awful.key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end),
    awful.key({ modkey }, "o", awful.client.movetoscreen),
    awful.key({ modkey }, "u", function (c) c.ontop = not c.ontop end),
    awful.key({ modkey }, "x", function (c) c:kill() end),
    awful.key({ modkey, "Control" }, "f", awful.client.floating.toggle),
    awful.key({ modkey }, "i",
        function (c)
            local result = ""
            result = result .. "<b>   Layout :</b> " .. awful.layout.getname(awful.layout.get(client.focus.screen)) .. "\n"
            result = result .. "<b>   Name :</b> " .. c.name .. "\n"
            result = result .. "<b>   Class :</b> " .. c.class .. "\n"
            result = result .. "<b>   Instance :</b> " .. c.instance .. "\n"
            if c.role then
                result = result .. "<b>    Role :</b> " .. c.role .. "\n"
            else
                result = result .. "<b>    Role :</b> None\n"
            end
            result = result .. "<b>      Type :</b> " .. c.type .. "\n"
            result = result .. "<b>      PID :</b> " .. c.pid .. "\n"
            result = result .. "<b>      XID :</b> " .. c.window .. "\n"
            result = result .. "<b>      Screen :</b> " .. c.screen .. "\n"
            result = result .. "<b>      Geometry :</b> " .. c:geometry().x .. "," .. c:geometry().y .. "," .. c:geometry().width .. "," .. c:geometry().height
            local appicon = ""
            if c.icon then
                appicon = c.icon
            else
                appicon = icons .. "/kAwOkenWhite/clear/22x22/actions/info2.png"
            end
              naughty.notify({
                title = "Info App",
                text = result,
                font = "Terminus 10",
                icon = appicon,
            })
        end)
)
-- }}

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule
    { rule = { },
      properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        keys = clientkeys,
        maximized_vertical   = false,
        maximized_horizontal = false,
        buttons = clientbuttons,
        size_hints_honor = false }
    },
}
-- }}}

-- {{{ Signals
-- Functions to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    -- Prevent overlap and off screen
    if not startup and not c.size_hints.user_position
       and not c.size_hints.program_position then
        awful.placement.no_overlap(c)
        awful.placement.no_offscreen(c)
    end
end)

-- Manage features when a client get the focus
client.connect_signal("focus",
    function(c)
        -- No border for maximized clients, full screen flash content and special ones
        if c.maximized_horizontal == true and c.maximized_vertical == true then
            c.border_color = beautiful.border_normal
        elseif c.name == "plugin-container" then
            local flash_client = c
            mt = timer({timeout=0.1})
            mt:connect_signal("timeout",function() c.fullscreen = true
            mt:stop() end)
            mt:start()
        -- Unless they are special ones
        elseif c.class == "Conky" or c.class == "Kruler" or c.class == "krunner" then
            c.border_width = 0
        else
            c.border_color = beautiful.border_focus
        end
    end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- Arrange signal handler
for s = 1, screen.count() do
    screen[s]:connect_signal("arrange", function ()
        local clients = awful.client.visible(s)
        local layout  = awful.layout.getname(awful.layout.get(s))

        if #clients > 0 then -- Fine grained borders and floaters control
            for _, c in pairs(clients) do
                -- Floaters almost always have borders
                if (awful.client.floating.get(c) or layout == "floating") and (c.class ~= "Conky" and c.class ~= "Kruler" and c.class ~= "krunner") then
                    c.border_width = beautiful.border_width
                -- Unless they are special ones
                elseif c.class == "Conky" or c.class == "Kruler" or c.class == "krunner" then
                    c.border_width = 0
                -- No borders with only one visible client
                elseif #clients == 1 or layout == "max" then
                    clients[1].border_width = 0
                else
                    c.border_width = beautiful.border_width
                end
            end
        end
    end)
end
-- }}}
