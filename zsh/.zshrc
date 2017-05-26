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
# Source Zplug, or offer to download it.
if [[ -z $ZPLUG_HOME ]]; then
    export ZPLUG_HOME=~/.config/zsh/zplug
fi
if [[ ! -f "${ZPLUG_HOME}/init.zsh" ]]; then
    vared -p 'Would you like to install zplug? (Y/N): ' -c choice
    if [[ $choice = y* || $choice = Y* ]]; then
        source zplug-installer/installer.zsh
    else
        echo "Things will fail."
    fi
fi
source "${ZPLUG_HOME}/init.zsh"

# Manage.py completions
zplug "plugins/django", \
    from:oh-my-zsh

# Faster git completion
zplug "plugins/gitfast", \
    from:oh-my-zsh

# Pass completion
zplug "plugins/pass", \
    from:oh-my-zsh

# Pip completion
zplug "plugins/pip", \
    from:oh-my-zsh

# Python completion
zplug "plugins/python", \
    from:oh-my-zsh

# Warp directories
zplug "plugins/wd", \
    from:oh-my-zsh

# Vim-like keybindings with some modifications
zplug "plugins/vi-mode", \
    from:oh-my-zsh

#OS-specific packages
zplug "plugins/archlinux", \
    from:oh-my-zsh, \
    if:"[[ -x /usr/bin/pacman ]]"

zplug "plugins/yum", \
    from:oh-my-zsh, \
    if:"[[ -x /usr/bin/yum ]]"

zplug "plugins/dnf", \
    from:oh-my-zsh, \
    if:"[[ -x /usr/bin/dnf ]]"

zplug "plugins/debian", \
    from:oh-my-zsh, \
    if:"[[ -x /usr/bin/apt-get ]]"


# External Bundles
# Open a pull request right there
zplug "caarlos0/zsh-open-pr"

# Add upstream remote to git like `add-upstream username`
zplug "caarlos0/zsh-add-upstream"

# Prettier version of l, with git support
zplug "rimraf/k"

# So many scripts
zplug "skx/sysadmin-util"

# Back up to directory name
zplug "Tarrasch/zsh-bd"

# So many colors "echo I am red | red" or "red hi"
zplug "Tarrasch/zsh-colors"

# Add a .gitignore based on a template
zplug "voronkovich/gitignore.plugin.zsh"

# Tons and tons of completions
zplug "zsh-users/zsh-completions", \
    use:"src"

# Pretty colors
zplug "zsh-users/zsh-syntax-highlighting", \
    defer:2

# Manage itself
zplug 'zplug/zplug', \
    hook-build:'zplug --self-manage'

# Load the theme.
zplug "robbyrussell/oh-my-zsh"
setopt prompt_subst # Make sure prompt is able to be generated properly.
zplug "caiogondim/bullet-train.zsh", \
    use:bullet-train.zsh-theme

if ! zplug check; then
    zplug install
fi

zplug load

# History tweaks
mkdir -p "${HOME}/.local/share/zsh"
HISTFILE="${HOME}/.local/share/zsh/history"
HISTSIZE=1000000000
SAVEHIST=$HISTSIZE
setopt inc_append_history # Append every command to $HISTFILE immediately
setopt share_history      # Always import new commands from $HISTFILE
setopt extended_history   # Save additional info to $HISTFILE
setopt hist_ignore_space  # Ignore history beginning with a space
setopt NO_BEEP

#ZSH Colorful stuff
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern root)

# Eliminate escape delay
KEYTIMEOUT=1

# Allow new features, i.e. ^ which negates the pattern following it
# ls <100-200>.txt, **/, and more
setopt extendedglob

#If pattern for filename generation has no matches, print an error.
setopt nomatch

# Disable changing the window title
export DISABLE_AUTO_TITLE=true

bindkey '^R' history-incremental-search-backward

say() { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

gpr() { git push origin HEAD && open-pr "$*" }

alias ip="ip -h -c"                                     # This too
alias ls="ls --color=auto"

export LESS="-R"

function sudoedit() {
    SUDO_COMMAND="sudoedit $@" command sudoedit "$@"
}

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

autoload -Uz manydots-magic
manydots-magic

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort access
zstyle ':completion:*' ignore-parents parent .. directory
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle :compinstall filename ${HOME}/.config/zsh/.zshrc

autoload -Uz compinit
compinit
# End of lines added by compinstall
