# ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx &

# Startup programs
compton --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc --unredir-if-possible --no-fading-openclose &
unclutter -root &
nm-applet &
alacritty -t ncmpcpp -e /bin/sh -c 'tmux a -t ncmpcpp || tmux new-session -s ncmpcpp -- ncmpcpp' &
alacritty -t neomutt -e /bin/sh -c 'tmux a -t neomutt || tmux new-session -s neomutt -- neomutt' &
alacritty -t default -e /bin/sh -c 'tmux a -t default || tmux new-session -s default' &
