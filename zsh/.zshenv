# Include go and cargo in path
export GOPATH=$HOME/.go

export PATH=$PATH:~/bin:${GOPATH//://bin:}/bin:${HOME}/.cargo/bin

export EDITOR=vim
export VISUAL=vim
export ALTERNATIVE_EDITOR=vi
alias :e=vim
alias :E=sudoedit

# Use UTF-8
export LANG=en_US.UTF-8

if [[ -r ~/.config/zsh/local.zsh ]]; then
        source ~/.config/zsh/local.zsh
fi
