POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND='15'
POWERLEVEL9K_DIR_HOME_FOREGROUND='15'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND='15'
POWERLEVEL9K_DIR_HOME_ETC_FOREGROUND='15'
POWERLEVEL9K_CONTEXT_DEFAULT_FOREGROUND='black';
POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND='10';
POWERLEVEL9K_CONTEXT_REMOTE_BACKGROUND='9';
POWERLEVEL9K_CONTEXT_REMOTE_FOREGROUND='black';
POWERLEVEL9K_STATUS_FOREGROUND='15'

POWERLEVEL9K_STATUS_CROSS=true
POWERLEVEL9K_STATUS_OK=false
POWERLEVEL9K_ALWAYS_SHOW_CONTEXT=true
POWERLEVEL9K_DIR_SHOW_WRITABLE=true

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
status
context
dir
vcs
virtualenv
background_jobs
custom_nomad_status
)

POWERLEVEL9K_DISABLE_RPROMPT=true
POWERLEVEL9K_CUSTOM_NOMAD_STATUS="get_nomad_env"

POWERLEVEL9K_CUSTOM_NOMAD_STATUS_BACKGROUND="007"
POWERLEVEL9K_CUSTOM_NOMAD_STATUS_FOREGROUND="darkgreen"

# Source Zplug, or offer to download it.
if [[ -z $ZPLUG_HOME ]]; then
    export ZPLUG_HOME=~/.config/zsh/zplug
fi
if [[ ! -f "${ZPLUG_HOME}/init.zsh" ]]; then
    vared -p 'Would you like to install zplug? (Y/N): ' -c choice
    if [[ $choice = y* || $choice = Y* ]]; then
        source ~/.config/zsh/zplug-installer/installer.zsh
    else
        echo "Things will fail."
    fi
fi
source "${ZPLUG_HOME}/init.zsh"

zplug "plugins/docker", \
    from:oh-my-zsh

zplug "plugins/docker-compose", \
    from:oh-my-zsh

# Faster git completion
zplug "plugins/gitfast", \
    from:oh-my-zsh

# Pip completion
zplug "plugins/pip", \
    from:oh-my-zsh

# Python completion
zplug "plugins/python", \
    from:oh-my-zsh

# Vim-like keybindings with some modifications
zplug "plugins/vi-mode", \
    from:oh-my-zsh

zplug "plugins/dnf", \
    from:oh-my-zsh, \
    if:"[[ -x /usr/bin/dnf ]]"

# External Bundles
# Add upstream remote to git like `add-upstream username`
zplug "caarlos0/zsh-add-upstream"

# So many scripts
zplug "skx/sysadmin-util"

# Add a .gitignore based on a template
zplug "voronkovich/gitignore.plugin.zsh"

zplug "walesmd/caniuse.plugin.zsh"

zplug "caarlos0/open-pr"

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
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

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

source "${HOME}/.config/zsh/alias.zsh"

autoload -Uz manydots-magic
manydots-magic

fpath+=~/.config/zsh/.zfunc

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

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/Downloads/google-cloud-sdk/path.zsh.inc" ]; then source "$HOME/Downloads/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/Downloads/google-cloud-sdk/completion.zsh.inc" ]; then source "$HOME/Downloads/google-cloud-sdk/completion.zsh.inc"; fi


get_nomad_env() {
	if [[ -v NOMAD_TOKEN ]] && [[ -v NOMAD_ADDR ]]; then
		if [[ $NOMAD_ADDR =~ "http://192.*" ]]; then
			echo " Vagrant"
		elif [[ $NOMAD_ADDR =~ "http://127.*" ]]; then
			echo " Local"
		elif [[ $NOMAD_ADDR =~ "https://.*" ]]; then
			echo " Prod"
		fi
	fi
}
