-- {{{ Required libraries
local gears     = require("gears")
local awful     = require("awful")
awful.rules     = require("awful.rules")
                  require("awful.autofocus")
local wibox     = require("wibox")
local beautiful = require("beautiful")
local naughty   = require("naughty")
local lain      = require("lain")
local keydoc    = require("keydoc")
local alttab    = require("alttab")
local ror    = require("aweror")
require("collision")()
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
--     Only awesome related. Other apps startup are managed by KDE.
function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

-- Compositor
-- run_once("compton -b --detect-rounded-corners --config " .. os.getenv("HOME") .. "/.config/awesome/compton.conf")

-- uncluter
run_once("unclutter")
-- run_once("rofi")
-- }}}

-- {{{ Variable definitions
-- localization
-- os.setlocale(os.getenv("LANG"))

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

-- Available tags
tags = {
   names = {"#"},
   layout = { layouts[1] }
}

-- Set tags and layouts for each attached screen
for s = 1, screen.count() do
   tags[s] = awful.tag(tags.names, s, tags.layout)
end

-- Clients
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}

-- {{{ Key bindings
-- {{ Global keys

globalkeys = awful.util.table.join(
    -- Assistance
    keydoc.group("Assistência"),

    awful.key({ modkey }, ",", function () awful.util.spawn(kdeconf) end, "Abre configurações do KDE*"),
    awful.key({ modkey, "Control" }, "r", awesome.restart, "Reinicia awesome"),

    awful.key({ modkey, "Shift" }, "u", awful.client.urgent.jumpto, "Foca próxima janela urgente"),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end, "Foca próxima tela"),

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
        end, "Alterna janelas* (shift para inverter)"),
    awful.key({ altkey,         }, "F2",
        function ()
            -- awful.util.spawn('rofi -show run')
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode runner')
        end, "Alterna janelas* (shift para inverter)"),

    -- Layout management (tile mode)
    keydoc.group("Layout (modo encaixe)"),
    awful.key({ modkey }, "a", function () awful.layout.arrange(client.focus.screen) end, "Aumenta largura mestre"),
    awful.key({ modkey, altkey }, "l", function () awful.tag.incmwfact(0.05) end, "Aumenta largura mestre"),
    awful.key({ modkey, altkey }, "h", function () awful.tag.incmwfact(-0.05) end, "Diminui largura mestre"),
    awful.key({ modkey, "Shift" }, "l", function () awful.tag.incnmaster(1) end, "Aumenta número de mestres*"),
    awful.key({ modkey, "Shift" }, "h", function () awful.tag.incnmaster(-1) end, "Diminui número de mestres*"),
    awful.key({ modkey, "Control" }, "l", function () awful.tag.incncol(1) end, "Aumenta número de colunas*"),
    awful.key({ modkey, "Control" }, "h", function () awful.tag.incncol(-1) end, "Diminui número de colunas*"),
    awful.key({ modkey, "Ctrl" }, "space", function ()
        awful.layout.inc(layouts, 1)
        naughty.notify({
          title = "Layout",
          text = awful.layout.getname(awful.layout.get(client.focus.screen)),
          font = "Terminus 10"
        })
        awful.layout.arrange(client.focus.screen)
     end, "Próximo layout*"),
    awful.key({ modkey, "Shift" }, "space", function ()
        awful.layout.inc(layouts, -1)
        naughty.notify({
          title = "Layout",
          text = awful.layout.getname(awful.layout.get(client.focus.screen)),
          font = "Terminus 10"
        })
        awful.layout.arrange(client.focus.screen)
     end, "Layout anterior"),
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1) end, "Troca com a próxima janela"),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end, "Troca com a janela anterior")
)
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(altkey))

-- {{ Client Keys
clientkeys = awful.util.table.join(
    keydoc.group("Janelas"),
    awful.key({ modkey }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end, "Minimiza"),
    awful.key({ modkey }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end, "Maximiza"),
   awful.key({ modkey }, "c",
        function (c)
            c:geometry({ x = 200,
                         y = 100,
                         width = 1304,
                         height = 688 })
        end, "Centraliza* (modo flutuante)"),
    awful.key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end, "Alterna para tela cheia"),
    awful.key({ modkey }, "o", awful.client.movetoscreen, "Move para outra tela"),
    awful.key({ modkey }, "u", function (c) c.ontop = not c.ontop end, "Sobe janela*"),
    awful.key({ modkey }, "x", function (c) c:kill() end, "Mata aplicação*"),
    awful.key({ modkey, "Control" }, "f", awful.client.floating.toggle, "Torna flutuante" ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end, "Coloca no mestre (modo encaixe)"),
    awful.key({ altkey }, "q",
        function (c)
            awful.util.spawn_with_shell('~/projects/shadow-go/shadow --mode time')
            -- local result = awful.util.pread("~/projects/ts_time/ts_time")
            -- naughty.notify({
              -- title = "Time",
              -- text = result,
              -- font = "Terminus 10"
            -- })
        end, "Time"),
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
        end, "Informações da janela do aplicativo*")
)
-- }}

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     maximized_vertical   = false,
                     maximized_horizontal = false,
                     buttons = clientbuttons,
                     size_hints_honor = false } },

    -- Match all new clients, notify name and class (for debug purposes only)
    -- { rule = { },
    --  properties = { },
    --  callback = function(c)
    --naughty.notify({title="New Client Debug", text="Name: ".. c.name.."\nClass: ".. c.class .. "\nScreen: ".. c.screen})
    --             end },

    -- {{ Application rules
    { rule = { instance = "Shadow" },
      properties = {
        focus = true,
        ontop = true,
        urgent = true
      },
      -- callback = function(c)
      --   naughty.notify({title="New Client Debug", text="Name: ".. c.name.."\nClass: ".. c.class .. "\nScreen: ".. c.screen})
      -- end
    },
    { rule = { class = "konsole" },
      properties = {
          maximized_vertical   = true,
          maximized_horizontal = true,
          screen = 2,
          x = 1920,
          tag = tags[2][1],
    } },
    { rule = { class = "google-chrome-unstable" },
      properties = {
          maximized_vertical   = true,
          maximized_horizontal = true,
    } },
    { rule = { class = "Yakuake" },
      properties = {
          x = 0,
          tag = tags[1][1],
          screen = 1
    } },
    { rule = { class = "Slack" },
      properties = {
          maximized_vertical   = true,
          maximized_horizontal = true,
          x = 1920,
          tag = tags[2][1],
          screen = 2
    } },
    { rule = { class = "Skype" },
      properties = {
          maximized_vertical   = true,
          -- maximized_horizontal = true,
          x = 1920,
          tag = tags[2][1],
          screen = 2
    } },
    { rule = { class = "Atom" },
      properties = {
          maximized_vertical   = true,
          maximized_horizontal = true,
    } } ,
    { rule = { class = "qutebrowser" },
      properties = {
          maximized_vertical   = true,
          maximized_horizontal = true,
    } },
    { rule = { class = "Thunderbird" },
      properties = {
          maximized_vertical   = true,
          -- maximized_horizontal = true,
          tag = tags[2][1],
    } },

    -- }}
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
