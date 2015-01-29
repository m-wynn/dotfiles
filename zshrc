setopt appendhistory extendedglob nomatch

source /usr/share/zsh/scripts/antigen/antigen.zsh

export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# ZSH Bundles
antigen bundle docker       #docker autocompletes
antigen bundle encode64     #encode and decode 64-bit
antigen bundle sudo         #Press Esc twice for sudo
antigen bundle tmux         #auto launch tmux
antigen bundle wd           #warp directories
antigen bundle web-search   #google from the command line

# External Bundles
antigen bundle adolfoabegg/browse-commit                #open latest commit in browser
antigen bundle skx/sysadmin-util                        #so many plugins
antigen-bundle Tarrasch/zsh-bd                          #back up to directory name
antigen-bundle Tarrasch/zsh-colors                      #so many colors "echo I am red | red" or "red hi"
antigen bundle unixorn/autoupdate-antigen.zshplugin     #auto update [every week]
antigen bundle voronkovich/gitignore.plugin.zsh         #add a .gitignore based on a template
antigen bundle walesmd/caniuse.plugin.zsh               #caniuse webgl
antigen bundle zsh-users/zsh-completions src            #tons and tons of completions
antigen bundle zsh-users/zsh-syntax-highlighting        #pretty colors

#Distro-specific bundles
case $(lsb_release -i | cut -d: -f2 | tr -d '\t') in
    Arch)
        antigen bundle archlinux    #pacman autocompletes
        antigen bundle systemd      #systemctl autocompletes and auto sudo
        source /usr/share/doc/pkgfile/command-not-found.zsh
        ;;
    Ubuntu)
        antigen bundle debian       #apt
        ;;
esac

# Load the theme.
antigen theme bira

# Tell antigen that you're done.
antigen apply

function say { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

ping() { grc --colour=auto /usr/bin/ping "$@" }


#Press Ctrl-Alt-Shift + direction to skip word by word
bindkey "^[[1;4C" forward-word
bindkey "^[[1;4D" backward-word