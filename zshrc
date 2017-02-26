# Bullet-train theme variables
BULLETTRAIN_PROMPT_SEPARATE_LINE=false
BULLETTRAIN_PROMPT_ADD_NEWLINE=false
BULLETTRAIN_CONTEXT_SHOW=true
BULLETTRAIN_TIME_SHOW=false
BULLETTRAIN_DIR_FG='15'
BULLETTRAIN_GIT_BG='15'
BULLETTRAIN_CONTEXT_FG=black;
BULLETTRAIN_STATUS_FG='15'

BULLETTRAIN_PROMPT_ORDER=(
    status
    context
    dir
    git
    virtualenv
)

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    BULLETTRAIN_CONTEXT_BG=9;
else
    BULLETTRAIN_CONTEXT_BG=10;
fi

#ZSH Colorful stuff
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern root)

# Source Zgen, wherever it may be.  Otherwise, offer to download it.
if [ -f /usr/share/zsh/scripts/zgen/zgen.zsh ]; then
    source /usr/share/zsh/scripts/zgen/zgen.zsh
else
    if [ ! -f ~/.zgen/zgen.zsh ]; then
        vared -p 'Would you like to install zgen? (Y/N): ' -c choice
        if [[ $choice = y* || $choice = Y* ]]; then
            mkdir -p ~/.zgen
            curl -L https://raw.githubusercontent.com/tarjoilija/zgen/master/zgen.zsh > ~/.zgen/zgen.zsh
        else
            echo "Things will fail."
        fi
    fi
    source ~/.zgen/zgen.zsh
fi
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc)

# History tweaks
export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt inc_append_history                # Append every command to $HISTFILE immediately
setopt share_history                        # Always import new commands from $HISTFILE
setopt extended_history                        # Save additional info to $HISTFILE
setopt hist_ignore_space                # Ignore history beginning with a space
setopt NO_BEEP

# Eliminate escape delay
KEYTIMEOUT=1

# Allow new features, i.e. ^ which negates the pattern following it, ls <100-200>.txt, **/, and more
setopt extendedglob

#If pattern for filename generation has no matches, print an error.
setopt nomatch

# Include go and cargo in path
export GOPATH=$HOME/.go

export PATH=$PATH:~/bin:${GOPATH//://bin:}/bin:${HOME}/.cargo/bin

# Disable changing the window title
export DISABLE_AUTO_TITLE=true

# Use vim
export EDITOR=vim
export VISUAL=vim
export ALTERNATIVE_EDITOR=vi
alias :e=vim
alias :E=sudoedit

# Use UTF-8
export LANG=en_US.UTF-8

if ! zgen saved; then
    echo "Creating a zgen save"

    # Load the oh-my-zsh's library.
    zgen oh-my-zsh

    # Oh-my-zsh Bundles
    zgen oh-my-zsh plugins/colored-man-pages # Colorizes man pages
    zgen oh-my-zsh plugins/django            # Manage.py completions
    zgen oh-my-zsh plugins/docker            # Docker autocompletes
    zgen oh-my-zsh plugins/encode64          # Encode and decode 64-bit
    zgen oh-my-zsh plugins/gitfast           # Faster git completion
    zgen oh-my-zsh plugins/pass              # Pass completion
    zgen oh-my-zsh plugins/pip               # Pip completion
    zgen oh-my-zsh plugins/python            # Python completion
    zgen oh-my-zsh plugins/rsync             # Rsync commands, like `rsync-copy`
    zgen oh-my-zsh plugins/wd                # Warp directories
    zgen oh-my-zsh plugins/vi-mode           # Vim-like keybindings with some modifications

    # External Bundles
    zgen load caarlos0/zsh-open-pr              # Open a pull request right there
    zgen load caarlos0/zsh-add-upstream         # Add upstream remote to git like `add-upstream username`
    zgen load chrissicool/zsh-256color          # Encourage 256 color mode
    zgen load rimraf/k                          # Prettier version of l, with git support
    zgen load skx/sysadmin-util                 # So many scripts
    zgen load Tarrasch/zsh-bd                   # Back up to directory name
    zgen load Tarrasch/zsh-colors               # So many colors "echo I am red | red" or "red hi"
    zgen load unixorn/autoupdate-zgen           # Automagic updates every week (by default)
    zgen load voronkovich/gitignore.plugin.zsh  # Add a .gitignore based on a template
    zgen load zsh-users/zsh-completions src     # Tons and tons of completions
    zgen load zsh-users/zsh-syntax-highlighting # Pretty colors

    # OS-specific bundles
    case $(uname -s) in
        Linux)
            if [ -x /usr/bin/pacman ]; then      # Arch
                zgen oh-my-zsh plugins/archlinux # Pacman autocompletes
                source /usr/share/doc/pkgfile/command-not-found.zsh
            elif [ -x /usr/bin/yum ]; then       # CentOS
                zgen oh-my-zsh plugins/yum       # Yum aliases
            elif [ -x /usr/bin/dnf ]; then       # New RHEL
                zgen oh-my-zsh plugins/dnf       # DNF aliases
            elif [ -x /usr/bin/apt-get ]; then   # Debian or Ubuntu
                zgen oh-my-zsh plugins/debian    # Apt
            fi
            pgrep systemd >/dev/null && \
                zgen oh-my-zsh plugins/systemd   # Systemctl autocompletes and auto sudo
            ;;
    esac

    # Load the theme.
    zgen load caiogondim/bullet-train-oh-my-zsh-theme bullet-train

    # Tell zgen that you're done.
    zgen save
fi

bindkey '^R' history-incremental-search-backward

say() { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

gpr() { git push origin HEAD && open-pr "$*" }

alias grep="grep --color=always"                        # Just watch this break things

alias ip="ip -h -c"                                     # This too

export LESS="-R"

if [[ "$TERM" != dumb ]] && (( $+commands[grc] )) ; then
    # Prevent grc aliases from overriding zsh completions.
    setopt COMPLETE_ALIASES

    # Supported commands
    cmds=(c++ cc configure cvs df diff dig gcc g++ ifconfig last ld ldap ldapadd ldapauth ldapdelete ldapmodify ldapmodrdn ldappassd ldapsearch ldapwhoami make mount mtr netstat ping ping6 ps traceroute traceroute6 wdiff );

    # Set alias for available commands.
    for cmd in $cmds ; do
        if (( $+commands[$cmd] )) ; then
            alias $cmd="grc --colour=auto $cmd"
        fi
    done

    # Clean up variables
    unset cmds cmd
fi

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' use-cache on                                                # Enable completion caching layer
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort access
zstyle ':completion:*' ignore-parents parent .. directory
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle :compinstall filename '/home/matthew/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
