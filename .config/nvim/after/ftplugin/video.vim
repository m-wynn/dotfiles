silent execute "!mpv " . shellescape(expand("%:p")) . " &>/dev/null &" | buffer# | bdelete# | redraw! | syntax on
