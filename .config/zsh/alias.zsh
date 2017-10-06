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


say() { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

gpr() { git push origin HEAD && open-pr "$*" }

alias ip="ip -h -c"                                     # This too
if [[ `uname` == 'Darwin' ]]; then
    alias ls="ls -G"
else
    alias ls="ls --color=auto"
fi
lc() { ~/bin/colorls/colorls.rb $1; }

if (( $+commands[exa] )) ; then
    alias ll="exa"
fi

alias conf='git --git-dir="${HOME}/.dotfiles/" --work-tree="$HOME"'

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
