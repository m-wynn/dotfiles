# change prefix to Ctrl-a (like in gnu-screen)
unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix


# shell
set -g default-command /bin/zsh
set -g default-shell /bin/zsh


# start with window 1 (instead of 0)
set -g base-index 1


# start with pane 1
set -g pane-base-index 1


# screen mode
set -g default-terminal "screen-256color"


# source config file
bind r source-file ~/.tmux.conf


# history ( scrollback buffer )
set -g history-limit 10000


# allow terminal scrolling

set-option -ga terminal-override ',rxvt-uni*:XT:Ms=\E]52;%p1%s;%p2%s\007'
set-window-option -g mode-mouse on


# super easy tab switching
bind -n S-Right next-window
bind -n S-Left previous-window
bind -n C-t new-window
bind -n S-Up command-prompt "rename-window %%"

# with mouse (click on pretty little boxes)
set -g mouse-select-window on

# splitting
unbind %
bind h split-window -h
unbind '"'
bind v split-window -v


# windows ^W w 
unbind ^W
bind -n ^W list-windows

# colon :
bind : command-prompt

# panes
set -g mouse-select-pane on
set -g pane-border-fg black
set -g pane-active-border-fg brightred

# resize panes with mouse (drag borders)
set -g mouse-select-pane on
set -g mouse-resize-pane on

# address vim mode switching delay
set-option -s escape-time 0

# tmux messages are displayed for 4 seconds
set-option -g display-time 4000

# status line
set -g status-utf8 on
set -g status-justify left
set -g status-bg default
set -g status-fg colour12

# refresh 'status-left' and 'status-right' more often
set -g status-interval 2


# messaging
set -g message-fg black
set -g message-bg yellow
set -g message-command-fg blue
set -g message-command-bg black


#window mode
setw -g mode-bg colour6
setw -g mode-fg colour0


# window status
setw -g window-status-current-format "#[fg=red]#[bg=white] #W "
setw -g window-status-format "#[bg=white]#[fg=blue] #W "
setw -g window-status-current-attr dim
setw -g window-status-attr reverse

# Use Xterm keys (mainly for C-left/right)
set -gw xterm-keys on

set -g status-position top 
set -g status-justify left
set -g status-left-length 0
set -g status-left ''

# vim like bindings
bind V send-keys "0v\$"
bind -t vi-copy V rectangle-toggle
bind Escape copy-mode
unbind p
bind p paste-buffer
bind -t vi-copy 'v' begin-selection
bind -t vi-copy 'y' copy-selection
bind -t vi-copy 'Space' halfpage-down
bind -t vi-copy 'Bspace' halfpage-up

# Info on right (or maybe left)
set -g status-right-length 60
set -g status-right ' #[fg=brightyellow]#(sensors -f| grep Physical | cut -d+ -f2 | cut -d. -f1)° #[fg=colour7]| #[fg=brightyellow]#(acpi -b | cut -d" " -f4 | cut -d"%" -f1)% #[fg=colour7]| #[fg=brightred]#(cut -d " " -f 1-3 /proc/loadavg)'


# loud or quiet?
set-option -g visual-activity off
set-option -g visual-bell on
set-option -g visual-content off
set-option -g visual-silence off
set-window-option -g monitor-activity on
set-window-option -g monitor-content on

# listen to alerts from all windows
set -g bell-action any


# some key-binding changes
bind x kill-pane
bind . next-layout
bind , previous-layout

# don't resize unless the other terminal is actually watching as well
setw -g aggressive-resize on