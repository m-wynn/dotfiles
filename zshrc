setopt inc_append_history extendedglob nomatch share_history

# Source Antigen, wherever it may be.  Otherwise, offer to download it.
if [ -f /usr/share/zsh/scripts/antigen/antigen.zsh ]; then
	source /usr/share/zsh/scripts/antigen/antigen.zsh
else
	if [ ! -f ~/.antigen/antigen.zsh ]; then
		vared -p 'Would you like to install antigen? (Y/N): ' -c choice 
		if [[ $choice = y* || $choice = Y* ]]; then
			mkdir -p ~/.antigen
			curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > ~/.antigen/antigen.zsh
		else
			echo "Things will fail."
		fi
	fi
	source ~/.antigen/antigen.zsh
fi

export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

export PATH=$PATH:~/bin:/root/.gem/ruby/2.2.0/bin
export NO_AT_BRIDGE=1
export DISABLE_AUTO_TITLE=true
export EDITOR=vim

export LANG=en_US.UTF-8
# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Oh-my-zsh Bundles
antigen bundle docker       # Docker autocompletes
antigen bundle encode64     # Encode and decode 64-bit
antigen bundle sudo         # Press Esc twice for sudo
antigen bundle tmux         # Auto launch tmux
antigen bundle wd           # Warp directories
antigen bundle web-search   # Google from the command line

# External Bundles
antigen bundle adolfoabegg/browse-commit                # Open latest commit in browser
antigen bundle caarlos0/zsh-open-pr			# Open a pull request right there
antigen bundle caarlos0/zsh-add-upstream		# Add upstream remote to git like `add-upstream username`
antigen bundle horosgrisa/mysql-colorize		# Colorize MySQL plugins
antigen bundle marzocchi/zsh-notify			# Notifications for non-zero exits or long commands
antigen bundle skx/sysadmin-util                        # So many scripts
antigen-bundle Tarrasch/zsh-bd                          # Back up to directory name
antigen-bundle Tarrasch/zsh-colors                      # So many colors "echo I am red | red" or "red hi"
antigen bundle unixorn/autoupdate-antigen.zshplugin     # Auto update [every week]
antigen bundle voronkovich/gitignore.plugin.zsh         # Add a .gitignore based on a template
antigen bundle walesmd/caniuse.plugin.zsh               # CanIUse `caniuse webgl`
antigen bundle zsh-users/zsh-completions src            # Tons and tons of completions
antigen bundle zsh-users/zsh-syntax-highlighting        # Pretty colors

# OS-specific bundles
case $(uname -s) in
	Darwin)
		# When I use a mac regularly, I'll put something here
		;;
	Linux)
		if [ -x /usr/bin/pacman ]; then		# Arch
        		antigen bundle archlinux	# Pacman autocompletes
        		source /usr/share/doc/pkgfile/command-not-found.zsh
		elif [ -x /usr/bin/yum ]; then		# CentOS
			antigen bundle yum		# Yum aliases
		elif [ -x /usr/bin/apt-get ]; then	# Debian or Ubuntu
        		antigen bundle debian		# Apt
		fi
		pgrep systemd >/dev/null && \
			antigen bundle systemd		# Systemctl autocompletes and auto sudo
		;;
    	OpenBSD)
		# We totally need pkg_add plugins...
        	;;
esac

# Powerline theme settings
#POWERLINE_RIGHT_A="exit-status"
POWERLINE_DISABLE_RPROMPT="true"
POWERLINE_NO_BLANK_LINE="true"
POWERLINE_DETECT_SSH="true"

#ZSH Colorful stuff

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern root)

# Load the theme.
antigen theme jeremyFreeAgent/oh-my-zsh-powerline-theme powerline
# Tell antigen that you're done.
antigen apply



say() { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }


gpr() {	  git push origin HEAD && open-pr "$*"  }	# Push and open a PR like that!

#Press Ctrl-Alt-Shift + direction to skip word by word
bindkey "^[[1;4C" forward-word
bindkey "^[[1;4D" backward-word

alias grep="grep --color=always"			# Just watch this break things

man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}

export LESS="-R"

GRC=`which grc`
if [ "$TERM" != dumb ] && [ -n GRC ]
then
  alias colorize="$GRC -es --colour=auto"
	for c in as c++ configure cvs df diff dig esperanto gas gcc g++ ld ldapadd ldapauth ldapdelete ldapmodify ldapmodrdn ldappassd ldapsearch ldapwhoami last make mount netstat ping php ps proftpd traceroute wdiff; do
		alias ${c}="colorize ${c}"
	done
fi
