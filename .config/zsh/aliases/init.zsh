man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        PAGER="${commands[less]:-$PAGER}" \
        _NROFF_U=1 \
        PATH="$HOME/bin:$PATH" \
        man "$@"
}

# Make aliases work in Sudo
alias sudo="sudo "

alias k="kubectl"

say() { mplayer "http://translate.google.com/translate_tts?tl=en&q=$1"; }

gpr() { git push origin HEAD && open-pr "$*" }


if [[ `uname` == 'Darwin' ]]; then
    alias ls="ls -G"
else
    alias ls="ls --color=auto"
fi

if (( $+commands[exa] )) ; then
    alias ls="exa"
fi

if (( $+commands[batgrep] )) ; then
    alias bgrep="batgrep"
fi

if (( $+commands[batman] )) ; then
    alias man="batman"
fi

if (( $+commands[batwatch] )) ; then
    alias bw="batwatch"
fi

if (( $+commands[prettybat] )) ; then
    alias pbat="prettybat"
fi

alias conf='git --git-dir="${HOME}/.dotfiles/" --work-tree="$HOME"'

export LESS="-R"

function sudoedit() {
    SUDO_COMMAND="sudoedit $@" command sudoedit "$@"
}


# if [[ "$TERM" != dumb ]] && (( $+commands[grc] )) ; then
#     # Prevent grc aliases from overriding zsh completions.
#     setopt COMPLETE_ALIASES

#     # Supported commands
#     cmds=(blkid c++ cc configure cvs df diff dig env fdisk findmnt free g++ gcc getfacl getsebool id ifconfig iostat ip iptables journalctl last ld ldap ldapadd ldapauth ldapdelete ldapmodify ldapmodrdn ldappassd ldapsearch ldapwhoami lsattr lsblk lsmod lsof lspci make mount mtr netstat nmap ping ping6 ps sar semanage sysctl systemctl traceroute traceroute6 ulimit uptime vmstat w wdiff);

#     # Set alias for available commands.
#     for cmd in $cmds ; do
#         if (( $+commands[$cmd] )) ; then
#             alias $cmd="grc --colour=auto $cmd"
#         fi
#     done

#     # Clean up variables
#     unset cmds cmd
# fi
