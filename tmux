#Only remote stuff
if-shell '[ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]' 'unbind C-b; set-option -g prefix C-a; bind-key C-a send-prefix'

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
set-option -g -q mouse on
bind-key -T root PPage if-shell -F "#{alternate_on}" "send-keys PPage" "copy-mode -e; send-keys PPage"
bind-key -t vi-copy PPage page-up
bind-key -t vi-copy NPage page-down

# super easy tab switching using Shift-[direction]
if-shell '[ -z "$SSH_CLIENT" ] && [ -z "$SSH_TTY" ]' 'bind -n S-Right next-window; bind -n S-Left previous-window; bind -n C-t new-window; bind -n S-Up command-prompt "rename-window %%"; bind-key -n C-Left swap-window -t -1; bind-key -n C-Left swap-window -t -1; bind-key -n C-Right swap-window -t +1'

# super easy tab switching using Alt-[direction]
if-shell '[ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ] && [ "$HOSTNAME" != "carbon" ]' 'bind -n M-Right next-window; bind -n M-Left previous-window; bind -n M-t new-window; bind -n M-Up command-prompt "rename-window %%"; bind-key -n C-M-Left swap-window -t -1; bind-key -n C-M-Right swap-window -t +1'

#Not so easy tab switching with Prefix-[direction]
if-shell '[ "$HOSTNAME" = "carbon" ]' 'bind Right next-window; bind Left previous-window; bind t new-window; bind Up command-prompt "rename-window %%"; bind-key -n C-Left swap-window -t -1; bind-key C-Right swap-window -t +1'

# splitting
unbind %
bind h split-window -h
unbind '"'
bind v split-window -v


# colon :
bind : command-prompt

# panes
set -g pane-border-fg "colour8"
set -g pane-active-border-fg "colour10"

# address vim mode switching delay
set-option -s escape-time 0

# tmux messages are displayed for 4 seconds
set-option -g display-time 4000

# status line
set -g status on
set -g status-utf8 on
set -g status-justify "left"
set -g status-bg "black"
set -g status-fg "default"
set -g status-position top

# refresh 'status-left' and 'status-right' more often
set -g status-interval 2


# messaging
set -g message-fg "colour15"
set -g message-bg "colour8"
set -g message-command-fg "colour15"
set -g message-command-bg "colour238"


#window mode
setw -g mode-bg colour6
setw -g mode-fg colour0


# window status
setw -g window-status-current-format "#[fg=black,bg=colour9,nobold,nounderscore,noitalics]#[fg=black,bg=colour9] #I #[fg=black,bg=colour9] #W #[fg=colour9,bg=black,nobold,nounderscore,noitalics]"
setw -g window-status-format "#[fg=colour255,bg=black] #I #[fg=colour255,bg=black] #W "
setw -g window-status-current-attr dim
setw -g window-status-attr none				#reverse
setw -g window-status-fg "colour255"
setw -g window-status-bg "black"
setw -g window-status-separator ""
setw -g window-status-activity-attr "none"
setw -g window-status-activity-fg "colour10"
setw -g window-status-activity-bg "black"

# Info on right
set -g status-right-length "100"
set -g status-right-attr "none"
set -g status-attr "none"
set -g status-right '#[fg=colour8,bg=black,nobold,nounderscore,noitalics]#[fg=colour15,bg=colour8] #(sensors -f| grep Physical | cut -d+ -f2 | cut -d. -f1)°  #(cut -d " " -f 1-3 /proc/loadavg) #[fg=colour3,bg=colour8,nobold,nounderscore,noitalics]#[fg=black,bg=colour3] #h '
if-shell '[ "$HOSTNAME" = "carbon" ] ' 'set -g status-right ""; set -g status-right-length 0; set -g status off'

# Use Xterm keys (mainly for C-left/right)
set -gw xterm-keys on

set -g status-left-length "100"
set -g status-left "#[fg=colour22,bg=colour10] #S #[fg=colour10,bg=black,nobold,nounderscore,noitalics]"
set -g status-left-attr "none"

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


# loud or quiet?
set-option -g visual-activity off
set-option -g visual-bell off
set-option -g visual-silence off
set-window-option -g monitor-activity on

# listen to alerts from all windows
set -g bell-action any


# some key-binding changes
bind x kill-pane
bind . next-layout
bind , previous-layout

# don't resize unless the other terminal is actually watching as well
setw -g aggressive-resize on

