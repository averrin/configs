--[[
     KDEsome awesome WM config
     github.com/denydias/kdesome
     based on github.com/copycat-killer/awesome-copycats Powerarrow Darker
--]]

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
-- local rodentbane = require("rerodentbane")
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
-- }}}
run_once("rofi")

-- {{{ Variable definitions
-- localization
os.setlocale(os.getenv("LANG"))

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
aweconf     = "kate --new --name aweconf " .. home .. "/.conkyrc " ..
              config_dir .. "/compton.conf " .. theme_dir .. "/theme.lua "
              .. config_dir .. "/rc.lua"
menutheme   = "sed 's/xdgmenu = {/xdgmenu = { theme = { height = 16, width = 300 },/'"
menugen     = "xdg_menu --format awesome | " .. menutheme .. " > " .. config_dir .. "/xdg_menu.lua"
-- }}}

-- {{{ Layouts and Tags
-- Available layouts
local layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
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

-- awesome_alttab
alttab.settings.preview_box = true
alttab.settings.preview_box_bg = "#1F1F1FAA"
alttab.settings.preview_box_border = "#1F1F1F00"
alttab.settings.preview_box_fps = 30
alttab.settings.preview_box_delay = 100
alttab.settings.client_opacity = false
-- }}}

-- {{{ Wibox
markup = lain.util.markup

-- Textclock
clockicon = wibox.widget.imagebox(beautiful.widget_clock)
mytextclock = awful.widget.textclock(" %a %d %b %H:%M")

-- calendar
lain.widgets.calendar:attach(mytextclock, { font_size = 10 })

-- File System Notification
-- fswidget = lain.widgets.fs()

-- {{ Separators
-- Spacer
spr = wibox.widget.textbox(' ')
-- Left pointing arrow
arrl = wibox.widget.imagebox()
arrl:set_image(beautiful.arrl)
-- Left pointing, lighter at left, darker at right arrow
arrl_dl = wibox.widget.imagebox()
arrl_dl:set_image(beautiful.arrl_dl)
-- Left pointing, darker at left, lighter at right arrow
arrl_ld = wibox.widget.imagebox()
arrl_ld:set_image(beautiful.arrl_ld)
-- Right pointing arrow
arrl_r = wibox.widget.imagebox()
arrl_r:set_image(beautiful.arrl_r)
-- }}

-- Create a wibox for each screen and add it
mywibox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end))
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 2, screen.count() do

    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                            awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                            awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end),
                            awful.button({ }, 6, function () awful.layout.inc(layouts, 1) end)))

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    -- if s >= 1 then
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = 18 })

    -- Widgets that are aligned to the upper left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(spr)
    left_layout:add(mytaglist[s])
    -- left_layout:add(arrl_dl)
    left_layout:add(spr)

    -- Widgets that are aligned to the upper right
    -- If you are moving widgets from a section with light grey background to dark grey or vice versa,
    -- use a replacement icon as appropriate from themes/powerarrow-darker/alticons so your icons match the bg.
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(spr)
    right_layout:add(arrl)
    right_layout:add(spr)
    right_layout:add(wibox.widget.systray())
    right_layout:add(spr)
    right_layout:add(arrl_ld)
    right_layout:add(arrl_dl)
    -- right_layout:add(mytextclock)
    right_layout:add(mytextclock)
    right_layout:add(spr)
    right_layout:add(arrl_ld)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)
    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
-- Root window
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
    )
)

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
    awful.key({ altkey }, "c", function () lain.widgets.calendar:show(7) end, "Mostra calendário*"),
    awful.key({ modkey, "Control" }, "r", awesome.restart, "Reinicia awesome"),
    awful.key({ modkey }, "q", rerodentbane, "rere..."),

    -- Focus management
    -- keydoc.group("Foco"),
    -- awful.key({ modkey,           }, "Tab",
    --     function ()
    --         awful.client.focus.history.previous()
    --         if client.focus then
    --             client.focus:raise()
    --         end
    --     end, "Alterna para janela focada anteriormente"),
    -- awful.key({ modkey }, "u", awful.client.urgent.jumpto, "Foca próxima janela urgente"),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end, "Foca próxima tela"),

    -- awesome_alttab
    -- Forward
    awful.key({ altkey,         }, "Tab",
        function ()
            -- alttab.switch(1, "Alt_L", "Tab", "ISO_Left_Tab")
            awful.util.spawn('rofi -show window')
        end, "Alterna janelas* (shift para inverter)"),
    awful.key({ altkey,         }, "F2",
        function ()
            -- alttab.switch(1, "Alt_L", "Tab", "ISO_Left_Tab")
            awful.util.spawn('rofi -show run')
        end, "Alterna janelas* (shift para inverter)"),
    -- -- Reverse
    -- awful.key({ altkey, "Shift"   }, "Tab",
    --     function ()
    --         alttab.switch(-1, "Alt_L", "Tab", "ISO_Left_Tab")
    --     end),
    --
    -- Layout management (tile mode)
    keydoc.group("Layout (modo encaixe)"),
    awful.key({ modkey }, "l", function () awful.tag.incmwfact(0.05) end, "Aumenta largura mestre"),
    awful.key({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end, "Diminui largura mestre"),
    awful.key({ modkey, "Shift" }, "l", function () awful.tag.incnmaster(1) end, "Aumenta número de mestres*"),
    awful.key({ modkey, "Shift" }, "h", function () awful.tag.incnmaster(-1) end, "Diminui número de mestres*"),
    awful.key({ modkey, "Control" }, "l", function () awful.tag.incncol(1) end, "Aumenta número de colunas*"),
    awful.key({ modkey, "Control" }, "h", function () awful.tag.incncol(-1) end, "Diminui número de colunas*"),
    awful.key({ modkey, "Ctrl" }, "space", function () awful.layout.inc(layouts, 1) end, "Próximo layout*"),
    awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end, "Layout anterior"),
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1) end, "Troca com a próxima janela"),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end, "Troca com a janela anterior")
)
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(altkey))

-- { Navigation and tag management
-- B all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        keydoc.group("Navegação e Tags"),
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end,
                  i == 5 and "Mostra apenas tag #" or nil),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  i == 5 and "Mostra/esconde tag #" or nil),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.movetotag(tag)
                     end
                  end,
                  i == 5 and "Move janela para tag #" or nil),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.toggletag(tag)
                      end
                  end,
                  i == 5 and "Adiciona janela na tag #" or nil))
end

-- }
-- }}

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
            c:geometry({ x = 30,
                         y = 48,
                         width = 1304,
                         height = 688 })
        end, "Centraliza* (modo flutuante)"),
    awful.key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end, "Alterna para tela cheia"),
    awful.key({ modkey }, "o", awful.client.movetoscreen, "Move para outra tela"),
    awful.key({ modkey, "Control" }, "Up", function (c) c.ontop = not c.ontop end, "Sobe janela*"),
    awful.key({ modkey }, "x", function (c) c:kill() end, "Mata aplicação*"),
    awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle, "Torna flutuante" ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end, "Coloca no mestre (modo encaixe)"),
    awful.key({ altkey }, "q",
        function (c)
            local result = awful.util.pread("python /home/user/ts_time.py")
            naughty.notify({
              -- title = "Time",
              text = result,
              font = "Terminus 10"
            })
        end, "Time"),
    awful.key({ modkey }, "i",
        function (c)
            local result = ""
            result = result .. "<b>     Name :</b> " .. c.name .. "\n"
            result = result .. "<b>   Class :</b> " .. c.class .. "\n"
            result = result .. "<b>Instance :</b> " .. c.instance .. "\n"
            if c.role then
                result = result .. "<b>    Role :</b> " .. c.role .. "\n"
            else
                result = result .. "<b>    Role :</b> None\n"
            end
            result = result .. "<b>     Type :</b> " .. c.type .. "\n"
            result = result .. "<b>      PID :</b> " .. c.pid .. "\n"
            result = result .. "<b>      XID :</b> " .. c.window .. "\n"
            result = result .. "<b>      Screen :</b> " .. c.screen .. "\n"
            result = result .. "<b>Geometry :</b> " .. c:geometry().x .. "," .. c:geometry().y .. "," .. c:geometry().width .. "," .. c:geometry().height
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
    --                 naughty.notify({title="New Client Debug", text="Name: ".. c.name.."\nClass: ".. c.class .. "\nScreen: ".. c.screen})
    --             end },

    -- {{ Application rules
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
          maximized_horizontal = true,
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
          maximized_horizontal = true,
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
