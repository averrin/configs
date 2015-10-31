--[[
     KDEsome awesome WM config
     github.com/denydias/kdesome
     based on github.com/copycat-killer/awesome-copycats Powerarrow Darker
--]]

theme                               = {}

themes_dir                          = os.getenv("HOME") .. "/.config/awesome/themes/kdesome"
theme.wallpaper                     = themes_dir .. "/wall.png"

theme.font                          = "Terminus 8"
theme.fg_normal                     = "#DDDDFF"
theme.fg_focus                      = "#508ED8"
theme.fg_urgent                     = "#CC9393"
theme.bg_normal                     = "#1A1A1A"
theme.bg_focus                      = "#313131"
theme.bg_urgent                     = "#1A1A1A"
theme.border_width                  = "1"
theme.border_normal                 = "#3F3F3F"
theme.border_focus                  = "#7F7F7F"
theme.border_marked                 = "#CC9393"
theme.taglist_fg_focus              = "#508ED8"
theme.tasklist_fg_normal            = "#DDDDFF"
theme.tasklist_fg_focus             = "#508ED8"
theme.tasklist_bg_normal            = "#1A1A1A"
theme.tasklist_bg_focus             = "#313131"
theme.notify_fg                     = theme.fg_normal
theme.notify_bg                     = theme.bg_normal
theme.notify_border                 = theme.border_focus
theme.awful_widget_height           = 14
theme.awful_widget_margin_top       = 2
theme.menu_height                   = "16"
theme.menu_width                    = "140"

theme.help_icon                     = themes_dir .. "/icons/help.png"
theme.submenu_icon                  = themes_dir .. "/icons/submenu.png"
theme.taglist_squares_sel           = themes_dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel         = themes_dir .. "/icons/square_unsel.png"

theme.layout_tile                   = themes_dir .. "/icons/tile.png"
theme.layout_tilegaps               = themes_dir .. "/icons/tilegaps.png"
theme.layout_tileleft               = themes_dir .. "/icons/tileleft.png"
theme.layout_tilebottom             = themes_dir .. "/icons/tilebottom.png"
theme.layout_tiletop                = themes_dir .. "/icons/tiletop.png"
theme.layout_fairv                  = themes_dir .. "/icons/fairv.png"
theme.layout_fairh                  = themes_dir .. "/icons/fairh.png"
theme.layout_spiral                 = themes_dir .. "/icons/spiral.png"
theme.layout_dwindle                = themes_dir .. "/icons/dwindle.png"
theme.layout_max                    = themes_dir .. "/icons/max.png"
theme.layout_fullscreen             = themes_dir .. "/icons/fullscreen.png"
theme.layout_magnifier              = themes_dir .. "/icons/magnifier.png"
theme.layout_floating               = themes_dir .. "/icons/floating.png"

theme.arrl                          = themes_dir .. "/icons/arrl.png"
theme.arrl_dl                       = themes_dir .. "/icons/arrl_dl.png"
theme.arrl_ld                       = themes_dir .. "/icons/arrl_ld.png"
theme.arrl_r                        = themes_dir .. "/icons/arrl_r.png"

theme.widget_ac                     = themes_dir .. "/icons/ac.png"
theme.widget_battery                = themes_dir .. "/alticons/battery.png"
theme.widget_battery_low            = themes_dir .. "/alticons/battery_low.png"
theme.widget_battery_empty          = themes_dir .. "/alticons/battery_empty.png"

theme.tasklist_disable_icon         = true
theme.tasklist_floating             = ""
theme.tasklist_maximized_horizontal = ""
theme.tasklist_maximized_vertical   = ""

return theme
