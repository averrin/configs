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
-- run_once("xbindkeys")
-- run_once("~/.screenlayout/main.sh")
-- run_once("setxkbmap -layout 'us,ru' -option 'grp:ctrl_shift_toggle,grp_led:scroll,caps:escape'")
-- run_once("gxkb")

-- uncluter
run_once("unclutter")
run_once("killall plasmashell")
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

tyrannical.tags = {
    {
        name        = "float",
        init        = true,
        exclusive   = false,
        screen      = {1,2,3},
        layout      = awful.layout.suit.floating,
        class       = {
          "yakyak", "adom"
        },
        role = {"None"}
    } ,
    {
        name        = "left",
        init        = true,
        exclusive   = false,
        screen      = {1,2,3},
        layout      = awful.layout.suit.max,
        class       = {
          "Atom", "vivaldi-snapshot", "yakyak", "adom", "Emacs"
        }
    } ,
    {
        name        = "right",
        init        = true,
        exclusive   = false,
        screen      = 2,
        force_screen = true,
        layout      = awful.layout.suit.tile,
        class = {
          "skypeforlinux", "Thunderbird", "st-256color", "Slack"
        }
    }
}

tyrannical.properties.floating = {
  "yakyak", "adom"
}

-- Clients
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}

ror = {
  ["h"]={"yakyak", "yakyak" },
  -- ["a"]={"atom-beta","Atom"},
  ["s"]={"slack", "slack" },
  ["t"]={"skypeforlinux", "skypeforlinux"},
  ["n"]={"dolphin", "dolphin"},
  ["v"]={"vivaldi-snapshot", "Vivaldi-snapshot"},
  ["g"]={"telegram", "telegram"},
  ["e"]={"emacs", "Emacs"},
  ["a"]={"emacs", "Emacs"},
  ["w"]={"google-chrome", "Google-chrome"},
  ["d"]={"dartium", "Chrome"},
}

globalkeys = awful.util.table.join(
    -- Assistance
    awful.key({ modkey }, ",", function () awful.util.spawn(kdeconf) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),

    -- Forward
    awful.key({ altkey,         }, "Tab",
        function ()
            -- awful.util.spawn_with_shell('~/projects/shadow-go/shadow')
            awful.util.spawn_with_shell('rofi -show combi -switchers combi -combi-modi window,run')
        end),
    awful.key({ altkey,         }, "F2",
        function ()
            -- awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode runner')
            awful.util.spawn_with_shell('rofi -show combi -switchers combi -combi-modi window,run')
        end),
    awful.key({ altkey }, "q",
        function (c)
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode time')
        end),
    awful.key({ modkey }, "l",
        function ()
            awful.util.spawn_with_shell('gnome-screensaver-command -l')
        end),

    awful.key({  }, '`', function ()
        pid = getpid(".ht.pid")
      local matcher = function (c)
        if c then
          itis = awful.rules.match(c, {pid = pid})
        else
          return
        end
        if itis then
          c:geometry({
            x = 0,
            y = 0,
            height = 500
          })
          c.maximized_horizontal = true
          c.floating = true
          return true
        else
          return false
        end
      end
      if matcher(client.focus) then
        client.focus.minimized = true
      else
        awful.client.run_or_raise('bash -c "st -e tmux & echo $! > ~/.ht.pid"', matcher)
      end
    end),
    awful.key({ altkey }, 'k', function ()
      pid = getpid(".ht_2.pid")
      local matcher = function (c)
        if c then
          awful.rules.match(c, {pid = pid})
          itis = awful.rules.match(c, {pid = pid})
          if itis then
            return true
          else
            return false
          end
        else
          return
        end
      end
      awful.client.run_or_raise('bash -c "st -e tmux & echo $! > ~/.ht_2.pid"', matcher)
  end),

    -- Layout management (tile mode)

    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end)
)

function getpid(f)
  file = io.open("/home/alexeynabrodov/" .. f)
  pid = "None"
  if file then
    pid = file:read "*a"
    pid = pid:match( "^%s*(.-)%s*$" )
    pid = tonumber(pid)
    file:close()
  end
  return pid
end

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
    awful.key({ "Control" }, '`',
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
            result = result .. "<b>   Layout :</b> " .. awful.layout.getname(awful.layout.get(client.focus.screen.index)) .. "\n"
            result = result .. "<b>   Name :</b> " .. c.name .. "\n"
            if c.class then
              result = result .. "<b>   Class :</b> " .. c.class .. "\n"
            end
            if c.ontop then
              result = result .. "<b>   On Top :</b> true\n"
            end
            if c.instance then
              result = result .. "<b>   Instance :</b> " .. c.instance .. "\n"
            end
            if c.role then
                result = result .. "<b>    Role :</b> " .. c.role .. "\n"
            else
                result = result .. "<b>    Role :</b> None\n"
            end
            result = result .. "<b>      Type :</b> " .. c.type .. "\n"
            result = result .. "<b>      PID :</b> " .. c.pid .. "\n"
            result = result .. "<b>      XID :</b> " .. c.window .. "\n"
            result = result .. "<b>      Screen :</b> " .. c.screen.index .. "\n"
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
    {rule = {role= "None"},
      properties = {
        width=100,
        hight=100
      }
  }
}
-- }}}

-- {{{ Signals
-- Functions to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- enable sloppy focus
    -- c:connect_signal("mouse::enter", function(c)
    --     if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    --         and awful.client.focus.filter(c) then
    --         client.focus = c
    --     end
    -- end)

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
