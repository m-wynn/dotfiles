# ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
# Adjust keyboard typematic delay and rate
# xset r rate 270 30
# Start Xorg server at this DPI
xrandr --dpi 90
# Merge & load configuration from .Xresources
# xrdb -merge ~/.Xresources
# xrdb -merge ~/.Xdefaults
xmodmap ~/.Xmodmap
# Let QT and GTK autodetect retina screen and autoadjust
# export QT_AUTO_SCREEN_SCALE_FACTOR=1
# export GDK_SCALE=2
# export GDK_DPI_SCALE=0.5
ibus-daemon -drx &

# Startup programs
compton --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc --unredir-if-possible --no-fading-openclose &
# unclutter -root &
nm-applet &
eval $(ssh-agent) &
# alacritty -t default -e /bin/sh -c 'tmux a -t default || tmux new-session -s default' &
# xautolock -detectsleep
#   -time 1 -locker "~/bin/i3blur.sh" \
#   -notify 30 \
#   -notifier "notify-send -u critical -t 10000 -- 'LOCKING screen in 30 seconds'" &
