zinit load zdharma-continuum/z-a-bin-gem-node
zinit load zdharma-continuum/z-a-as-monitor

zinit ice from"gh-r" as"program" atload'!eval $(starship init zsh)'
zinit load starship/starship

zinit wait lucid for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
      zdharma-continuum/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions

zinit ice wait lucid as"program" from"gh-r" \
  mv"zoxide*/zoxide -> zoxide" \
  atclone"./zoxide init zsh > init.zsh" \
  atpull"%atclone" src"init.zsh" nocompile'!'
zinit load ajeetdsouza/zoxide

# Completions
zinit wait"1" lucid has"kubectl" as"completion" atpull"%atclone" run-atpull for \
  id-as"kubectl_completion" \
    atclone"kubectl completion zsh > _kubectl" \
    zdharma-continuum/null \
  id-as"helm_completion" \
    atclone"helm completion zsh > _helm" \
    zdharma-continuum/null

zinit wait"1" lucid from"gh-r" as"null" for \
    sbin"**/delta"        @dandavison/delta \
    sbin"**/fd"           @sharkdp/fd \
    sbin"**/k9s"          @derailed/k9s \
    sbin"**/nvim"         @neovim/neovim \
    sbin"**/sk"           @lotabout/skim \
    sbin"**/terraform-ls" @hashicorp/terraform-ls 

zinit wait"1" lucid from"gh-r" as"command" atpull"%atclone" for \
    atclone"cp -vf bat*/autocomplete/bat.zsh _bat" \
    sbin"**/bat" @sharkdp/bat \
    \
    atclone"cp -vf completions/exa.zsh _exa" \
    sbin"**/exa" @ogham/exa \
    \
    atclone"cp -vf fd*/autocomplete/fd.zsh _fd" \
    sbin"**/fd" @sharkdp/fd \

zinit lucid wait"2" as"command" nocompletions for \
  id-as"eslint_d" node"eslint_d" zdharma-continuum/null \
  id-as"fixjson" node"fixjson" zdharma-continuum/null \
  id-as"nginxbeautifier" node"nginxbeautifier" zdharma-continuum/null \
  id-as"prettierd" node"prettierd" zdharma-continuum/null \
  id-as"checkov" pip"checkov <- !checkov -> checkov" zdharma-continuum/null \

# wait1 misc
zinit lucid wait"1" for \
  id-as"skim_completion" pick"shell/key-bindings.zsh" atclone"cp -vf shell/completion.zsh _sk" atpull"%atclone" @lotabout/skim \
    atclone"mkdir -p ~/.config/grc; cp -fv ./colourfiles/conf.* ./grc.conf ~/.config/grc" \
    atpull"%atclone" sbin"(grc|grcat)" ver"devel" garabik/grc \
  id-as"terraform" as"readurl|program" extract \
    dlink0"/terraform/%VERSION%/~%.*-(alpha|beta|rc).*%" \
    dlink"/terraform/%VERSION%/terraform_%VERSION%_linux_amd64.zip" \
    https://releases.hashicorp.com/terraform/

zinit ice lucid wait"1" id-as "terraform_completion" as"completion" \
    zinit snippet OMZP::terraform/_terraform


zinit ice wait"2" lucid
zinit load voronkovich/gitignore.plugin.zsh

zinit load %HOME/.config/zsh/aliases
