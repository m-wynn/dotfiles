setopt inc_append_history extendedglob nomatch share_history

# Source Antigen, wherever it may be.  Otherwise, offer to download it.
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
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)

export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

export PATH=$PATH:~/bin:/root/.gem/ruby/2.2.0/bin:/home/matthew/.gem/ruby/2.2.0/bin
export NO_AT_BRIDGE=1
export DISABLE_AUTO_TITLE=true
export EDITOR=vim

export LANG=en_US.UTF-8

if ! zgen saved; then
	echo "Creating a zgen save"

	# Load the oh-my-zsh's library.
	zgen oh-my-zsh

	# Oh-my-zsh Bundles
	zgen oh-my-zsh plugins/docker       # Docker autocompletes
	zgen oh-my-zsh plugins/encode64     # Encode and decode 64-bit
	zgen oh-my-zsh plugins/sudo         # Press Esc twice for sudo
	zgen oh-my-zsh plugins/tmux         # Auto launch tmux
	zgen oh-my-zsh plugins/wd           # Warp directories
	zgen oh-my-zsh plugins/web-search   # Google from the command line

	# External Bundles
	zgen load adolfoabegg/browse-commit                # Open latest commit in browser
	zgen load caarlos0/zsh-open-pr			# Open a pull request right there
	zgen load caarlos0/zsh-add-upstream		# Add upstream remote to git like `add-upstream username`
	zgen load horosgrisa/mysql-colorize		# Colorize MySQL plugins
	zgen load marzocchi/zsh-notify			# Notifications for non-zero exits or long commands
	zgen load skx/sysadmin-util                        # So many scripts
	zgen load Tarrasch/zsh-bd                          # Back up to directory name
	zgen load Tarrasch/zsh-colors                      # So many colors "echo I am red | red" or "red hi"
	#zgen load unixorn/autoupdate-antigen.zshplugin     # Auto update [every week]
	zgen load voronkovich/gitignore.plugin.zsh         # Add a .gitignore based on a template
	zgen load walesmd/caniuse.plugin.zsh               # CanIUse `caniuse webgl`
	zgen load zsh-users/zsh-completions src            # Tons and tons of completions
	zgen load zsh-users/zsh-syntax-highlighting        # Pretty colors

	# OS-specific bundles
	case $(uname -s) in
		Darwin)
			# When I use a mac regularly, I'll put something here
			;;
		Linux)
			if [ -x /usr/bin/pacman ]; then		# Arch
				zgen oh-my-zsh plugins/archlinux	# Pacman autocompletes
				source /usr/share/doc/pkgfile/command-not-found.zsh
			elif [ -x /usr/bin/yum ]; then		# CentOS
				zgen oh-my-zsh plugins/yum		# Yum aliases
			elif [ -x /usr/bin/apt-get ]; then	# Debian or Ubuntu
				zgen oh-my-zsh plugins/debian		# Apt
			fi
			pgrep systemd >/dev/null && \
				zgen oh-my-zsh plugins/systemd		# Systemctl autocompletes and auto sudo
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
	zgen load jeremyFreeAgent/oh-my-zsh-powerline-theme powerline
	# Tell zgen that you're done.
	zgen save
fi


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
	alias colorize="grc -es --colour=auto"
	for c in as c++ configure cvs df diff dig esperanto gas gcc g++ ld ldapadd ldapauth ldapdelete ldapmodify ldapmodrdn ldappassd ldapsearch ldapwhoami last make mount netstat ping php ps proftpd traceroute wdiff; do
		alias ${c}="colorize ${c}"
	done
fi
# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
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
