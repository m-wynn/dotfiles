eval `ssh-agent`
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx &
xrdb ~/.Xresources
xmodmap ~/.Xmodmap
mpd &
