--[[

	 Powerarrow Darker Awesome WM config 2.0
	 github.com/copycat-killer

--]]

theme					= {}

themes_dir				= os.getenv("HOME") .. "/.config/awesome/theme"
theme.wallpaper				= themes_dir .. "/wall.png"

theme.font				= "Tamsyn 10.5"
theme.taglist_font			= "Entypo Social 21"
theme.fg_normal				= "#C5C8C6"
theme.fg_focus				= "#85678F"
theme.fg_urgent				= "#A54242"
theme.bg_normal				= "#373b41"
theme.bg_focus				= "#282a2e"
theme.bg_urgent				= "#1d1f21"
theme.border_width			= "1"
theme.border_normal			= "#1c2022"
theme.border_focus			= "#606060"
theme.border_marked			= "#85678F"
theme.titlebar_bg_focus			= "#C5C8C6"
theme.titlebar_bg_normal		= "#C5C8C6"
theme.taglist_fg_focus			= "#85678F"
theme.tasklist_bg_focus			= "#282a2e"
theme.tasklist_fg_focus			= "#85678F"
theme.textbox_widget_margin_top		= 1
theme.notify_fg				= theme.fg_normal
theme.notify_bg				= theme.bg_normal
theme.notify_border			= theme.border_focus
theme.awful_widget_height		= 14
theme.awful_widget_margin_top		= 2
theme.mouse_finder_color		= "#CC9393"
theme.menu_height			= "16"
theme.menu_width			= "140"

theme.fg_black				= "#282a2e"
theme.fg_red				= "#a54242"
theme.fg_green				= "#8c9440"
theme.fg_yellow				= "#de935f"
theme.fg_blue				= "#5f819d"
theme.fg_magenta			= "#85678f"
theme.fg_cyan				= "#5e8d87"
theme.fg_white				= "#707880"

theme.bg_black				= "#373b41"
theme.bg_red				= "#cc6666"
theme.bg_green				= "#b5bd68"
theme.bg_yellow				= "#f0c674"
theme.bg_blue				= "#81a2be"
theme.bg_magenta			= "#b294bb"
theme.bg_cyan				= "#8abeb7"
theme.bg_white				= "#c5c8c6"

theme.submenu_icon			= themes_dir .. "/icons/submenu.png"
theme.taglist_squares_sel		= themes_dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel		= themes_dir .. "/icons/square_unsel.png"

theme.layout_tile			= themes_dir .. "/icons/tile.png"
theme.layout_tilegaps			= themes_dir .. "/icons/tilegaps.png"
theme.layout_tileleft			= themes_dir .. "/icons/tileleft.png"
theme.layout_tilebottom			= themes_dir .. "/icons/tilebottom.png"
theme.layout_tiletop			= themes_dir .. "/icons/tiletop.png"
theme.layout_fairv			= themes_dir .. "/icons/fairv.png"
theme.layout_fairh			= themes_dir .. "/icons/fairh.png"
theme.layout_spiral			= themes_dir .. "/icons/spiral.png"
theme.layout_dwindle			= themes_dir .. "/icons/dwindle.png"
theme.layout_max			= themes_dir .. "/icons/max.png"
theme.layout_fullscreen			= themes_dir .. "/icons/fullscreen.png"
theme.layout_magnifier			= themes_dir .. "/icons/magnifier.png"
theme.layout_floating			= themes_dir .. "/icons/floating.png"

theme.arrl				= themes_dir .. "/icons/arrl.png"
theme.arrl_dl				= themes_dir .. "/icons/arrl_dl.png"
theme.arrl_ld				= themes_dir .. "/icons/arrl_ld.png"

theme.widget_ac				= themes_dir .. "/icons/ac.png"
theme.widget_battery			= themes_dir .. "/icons/battery.png"
theme.widget_battery_low		= themes_dir .. "/icons/battery_low.png"
theme.widget_battery_empty		= themes_dir .. "/icons/battery_empty.png"
theme.widget_mem			= themes_dir .. "/icons/mem.png"
theme.widget_cpu			= themes_dir .. "/icons/cpu.png"
theme.widget_temp			= themes_dir .. "/icons/temp.png"
theme.widget_net			= themes_dir .. "/icons/net.png"
theme.widget_hdd			= themes_dir .. "/icons/hdd.png"
theme.widget_music			= themes_dir .. "/icons/note.png"
theme.widget_music_on			= themes_dir .. "/icons/note_on.png"
theme.widget_vol			= themes_dir .. "/icons/vol.png"
theme.widget_vol_low			= themes_dir .. "/icons/vol_low.png"
theme.widget_vol_no			= themes_dir .. "/icons/vol_no.png"
theme.widget_vol_mute			= themes_dir .. "/icons/vol_mute.png"
theme.widget_mail			= themes_dir .. "/icons/mail.png"
theme.widget_mail_on			= themes_dir .. "/icons/mail_on.png"

theme.tasklist_disable_icon		= true
theme.tasklist_floating			= ""
theme.tasklist_maximized_horizontal 	= ""
theme.tasklist_maximized_vertical	= ""

return theme
