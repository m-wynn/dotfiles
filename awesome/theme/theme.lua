--[[
						 
	 Powerarrow Darker Awesome WM config 2.0 
	 github.com/copycat-killer		 
						 
--]]

theme					= {}

themes_dir				= os.getenv("HOME") .. "/.config/awesome/theme"
theme.wallpaper				= themes_dir .. "/wall.png"

theme.font				= "Tamsyn 10.5"
theme.taglist_font			= "Entypo Social 21"
theme.fg_normal				= "#FFFFFF"
theme.fg_focus				= "#1976D2"
theme.fg_urgent				= "#E53935"
theme.bg_normal				= "#242424"
theme.bg_focus				= "#1A1A1A"
theme.bg_urgent				= "#000000"
theme.border_width			= "1"
theme.border_normal			= "#1c2022"
theme.border_focus			= "#606060"
theme.border_marked			= "#42A5F5"
theme.titlebar_bg_focus			= "#FFFFFF"
theme.titlebar_bg_normal		= "#FFFFFF"
theme.taglist_fg_focus			= "#1976D2"
theme.tasklist_bg_focus			= "#1A1A1A"
theme.tasklist_fg_focus			= "#1976D2"
theme.textbox_widget_margin_top		= 1
theme.notify_fg				= theme.fg_normal
theme.notify_bg				= theme.bg_normal
theme.notify_border			= theme.border_focus
theme.awful_widget_height		= 14
theme.awful_widget_margin_top		= 2
theme.mouse_finder_color		= "#CC9393"
theme.menu_height			= "16"
theme.menu_width			= "140"

theme.fg_black				= "#424242"
theme.fg_red				= "#ce5666"
theme.fg_green				= "#3dd37c"
theme.fg_yellow				= "#f1c40f"
theme.fg_blue				= "#3498db"
theme.fg_magenta			= "#9b59b6"
theme.fg_cyan				= "#1abc9c"
theme.fg_white				= "#bdc3c7"
theme.fg_blue				= "#2980b9"

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
