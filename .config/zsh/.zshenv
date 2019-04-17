# Include go and cargo in path
export GOPATH=$HOME/.go
export PATH="$PATH:$(yarn global bin)"

export PATH=$PATH:~/bin:${GOPATH//://bin:}/bin:${HOME}/.cargo/bin
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export EDITOR=nvim
export VISUAL=nvim
export ALTERNATIVE_EDITOR=vi
alias :e=nvim
alias :E=sudoedit

# Use UTF-8
export LANG=en_US.UTF-8

if [[ -r ~/.config/zsh/local.zsh ]]; then
        source ~/.config/zsh/local.zsh
fi
