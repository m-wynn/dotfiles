# ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx &

# Startup programs
compton --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc --unredir-if-possible --no-fading-openclose &
unclutter -root &
nm-applet &
alacritty -t default -e /bin/sh -c 'tmux a -t default || tmux new-session -s default' &
exec xautolock -detectsleep 
  -time 3 -locker "i3lock -d -c 000070" \
  -notify 30 \
  -notifier "notify-send -u critical -t 10000 -- 'LOCKING screen in 30 seconds'"
