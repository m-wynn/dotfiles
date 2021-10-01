export STARSHIP_CONFIG=~/.config/zsh/starship.toml
export FZF_MARKS_COMMAND="sk --height 40% --reverse"
export SKIM_DEFAULT_OPTIONS="--tiebreak=score,index"
export ZSH_AUTOSUGGEST_HISTORY_IGNORE="cd *|?(#c50,)"
source ~/.zinit/bin/zinit.zsh

zinit light zinit-zsh/z-a-bin-gem-node
zinit light zinit-zsh/z-a-readurl

# zinit ice wait notify
# zinit snippet OMZ::plugins/vi-mode

zinit wait lucid light-mode for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
      zdharma/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions \

zinit ice wait lucid
zinit load urbainvaes/fzf-marks

zinit ice wait"2" as"command" from"gh-r" lucid \
  mv"zoxide*/zoxide -> zoxide" \
  atclone"./zoxide init zsh > init.zsh" \
  atpull"%atclone" src"init.zsh" nocompile'!'
zinit light ajeetdsouza/zoxide

zinit ice from"gh-r" as"program" atload'!eval $(starship init zsh)'
zinit light starship/starship

zinit light-mode lucid wait has"kubectl" for \
  id-as"kubectl_completion" \
  as"completion" \
  atclone"kubectl completion zsh > _kubectl" \
  atpull"%atclone" \
  run-atpull \
    zdharma/null \
  id-as"helm_completion" \
  as"completion" \
  atclone"helm completion zsh > _helm" \
  atpull"%atclone" \
  run-atpull \
    zdharma/null

zinit wait"1" lucid id-as'terraform' as'readurl|command' extract \
    dlink0'/terraform/%VERSION%/~%.*-(alpha|beta|rc).*%' \
    dlink'/terraform/%VERSION%/terraform_%VERSION%_linux_amd64.zip' \
    for https://releases.hashicorp.com/terraform/

zinit wait"1" lucid from"gh-r" as"null" for \
    sbin"**/fd" @sharkdp/fd \
    sbin"**/bat" @sharkdp/bat \
    sbin"**/sk" @lotabout/skim \
    sbin"**/exa" ogham/exa \
    sbin"**/terraform-ls" @hashicorp/terraform-ls \
    sbin"**/nvim -> nvim" neovim/neovim \
    sbin"**/k9s -> k9s" derailed/k9s \
    sbin"**/delta -> delta" dandavison/delta \
    sbin"**/sad -> sad" bpick"*linux-gnu.zip" ms-jpq/sad \

zinit wait"1" lucid light-mode for \
    pick"shell/key-bindings.zsh" id-as"skim-full" lotabout/skim \
    atclone"mkdir -p ~/.config/grc; cp -fv ./colourfiles/conf.* ./grc.conf ~/.config/grc" \
        atpull"%atclone" sbin"(grc|grcat)" ver"devel" garabik/grc

zinit ice wait"2" lucid
zinit load voronkovich/gitignore.plugin.zsh

zinit light %HOME/.config/zsh/aliases

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line

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

# source "${HOME}/.config/zsh/alias.zsh"

# autoload -Uz manydots-magic
# manydots-magic

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
