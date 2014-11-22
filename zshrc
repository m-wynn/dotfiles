source /usr/share/zsh/scripts/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle encode64
antigen bundle archlinux
antigen bundle wd

# Load the theme.
antigen theme bira

# Tell antigen that you're done.
antigen apply

source /usr/share/doc/pkgfile/command-not-found.zsh

function say { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

ping() { grc --colour=auto /usr/bin/ping "$@" }
