-- {{{ Main
theme                               = {}
theme.confdir                       = os.getenv("HOME") .. "/.config/awesome/theme/"
theme.wallpaper                     = theme.confdir .. "/wall.png"
-- }}}


-- {{{ Fonts
theme.font                          = "Tamsyn 10.5"
theme.taglist_font                  = "Entypo Social 21"
-- }}}


-- {{{ Colors
theme.fg_normal                     = "#FFFFFF"
theme.fg_focus                      = "#0099CC"
theme.fg_urgent                     = "#af1d18"

theme.bg_normal                     = "#242424"
theme.bg_focus                      = "#1A1A1A"
theme.bg_urgent                     = "#000000"

--theme.fg_minimize                   = "#ffffff"
--theme.fg_black                      = "#424242"
--theme.fg_red                        = "#ce5666"
--theme.fg_green                      = "#3dd37c"
--theme.fg_yellow                     = "#f1c40f"
--theme.fg_blue                       = "#3498db"
--theme.fg_magenta                    = "#9b59b6"
--theme.fg_cyan                       = "#1abc9c"
--theme.fg_white                      = "#bdc3c7"
--theme.fg_blu                        = "#2980b9"
-- }}}

-- {{{ Borders
theme.border_width                  = "1"
theme.border_normal                 = "#1c2022"
theme.border_focus                  = "#606060"
theme.border_marked                 = "#3ca4d8"
-- }}}

theme.menu_submenu_icon             = theme.confdir .. "/icons/submenu.png"
theme.widget_temp                   = theme.confdir .. "/icons/temp.png"
theme.widget_uptime                 = theme.confdir .. "/icons/ac.png"
theme.widget_cpu                    = theme.confdir .. "/icons/cpu.png"
theme.widget_weather                = theme.confdir .. "/icons/dish.png"
theme.widget_fs                     = theme.confdir .. "/icons/fs.png"
theme.widget_mem                    = theme.confdir .. "/icons/mem.png"
theme.widget_fs                     = theme.confdir .. "/icons/fs.png"
theme.widget_note                   = theme.confdir .. "/icons/note.png"
theme.widget_note_on                = theme.confdir .. "/icons/note_on.png"
theme.widget_netdown                = theme.confdir .. "/icons/net_down.png"
theme.widget_netup                  = theme.confdir .. "/icons/net_up.png"
theme.widget_mail                   = theme.confdir .. "/icons/mail.png"
theme.widget_batt                   = theme.confdir .. "/icons/bat.png"
theme.widget_clock                  = theme.confdir .. "/icons/clock.png"
theme.widget_vol                    = theme.confdir .. "/icons/spkr.png"

theme.taglist_squares_sel           = theme.confdir .. "/icons/square_a.png"
theme.taglist_squares_unsel         = theme.confdir .. "/icons/square_b.png"

theme.tasklist_disable_icon         = true
theme.tasklist_floating             = ""
theme.tasklist_maximized_horizontal = ""
theme.tasklist_maximized_vertical   = ""

theme.layout_tile                   = theme.confdir .. "/icons/tile.png"
theme.layout_tilegaps               = theme.confdir .. "/icons/tilegaps.png"
theme.layout_tileleft               = theme.confdir .. "/icons/tileleft.png"
theme.layout_tilebottom             = theme.confdir .. "/icons/tilebottom.png"
theme.layout_tiletop                = theme.confdir .. "/icons/tiletop.png"
theme.layout_fairv                  = theme.confdir .. "/icons/fairv.png"
theme.layout_fairh                  = theme.confdir .. "/icons/fairh.png"
theme.layout_spiral                 = theme.confdir .. "/icons/spiral.png"
theme.layout_dwindle                = theme.confdir .. "/icons/dwindle.png"
theme.layout_max                    = theme.confdir .. "/icons/max.png"
theme.layout_fullscreen             = theme.confdir .. "/icons/fullscreen.png"
theme.layout_magnifier              = theme.confdir .. "/icons/magnifier.png"
theme.layout_floating               = theme.confdir .. "/icons/floating.png"

theme.mpd                           = theme.confdir .. "/icons/mpd.png"
theme.mpd_on                        = theme.confdir .. "/icons/mpd_on.png"


return theme
