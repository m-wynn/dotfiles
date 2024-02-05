{ config, pkgs, ... }:
{
  programs.wezterm = {
  };
  programs.tmux = {
    enable = true;
    baseIndex = 1;
    aggressiveResize = true;
    historyLimit = 30000;
    keyMode = "vi";
    # mouse = true; # unsupported yet?
    newSession = true;
    plugins = with pkgs; [
      tmuxPlugins.yank
      tmuxPlugins.tmux-thumbs
      { 
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "catppuccin";
          version = "4e48b09";
          src = pkgs.fetchFromGitHub {
            owner = "catppuccin";
            repo = "tmux";
            rev = "4e48b09a76829edc7b55fbb15467cf0411f07931";
            sha256 = "bXEsxt4ozl3cAzV3ZyvbPsnmy0RAdpLxHwN82gvjLdU=";
          };
        };
      }
    ];
    terminal = "tmux-256color";
    extraConfig = ''
    set-option -g -q mouse on
    set-option -ga terminal-overrides ",xterm-256color:RGB"
    set-option -sg escape-time 10
    set -g @catppuccin_flavour 'mocha'
    set -g status-position top

    bind x kill-pane
    bind r source-file "$HOME/.config/tmux/tmux.conf"

    unbind %
    bind h split-window -h -c "#{pane_current_path}"
    unbind '"'
    bind v split-window -v -c "#{pane_current_path}"

    if-shell '[ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]' \
    'unbind C-b;\
    set-option -g prefix C-a;\
    bind C-a send-prefix
    bind -n M-Right next-window; \
    bind -n M-Left previous-window; \
    bind -n M-t new-window "#{pane_current_path}"; \
    bind -n M-Up command-prompt "rename-window %%"; \
    bind -n C-M-Left swap-window -t -1; \
    bind -n C-M-Right swap-window -t +1'
    if-shell '[ -z "$SSH_CLIENT" ] && [ -z "$SSH_TTY" ]' \
    'bind -n S-Right next-window; \
    bind -n S-Left previous-window; \
    bind -n C-t new-window -c "#{pane_current_path}"; \
    bind -n S-Up command-prompt "rename-window %%"; \
    bind -n C-Left swap-window -t -1; \
    bind -n C-Left swap-window -t -1; \
    bind -n C-Right swap-window -t +1'
    '';
  };
  programs.kitty = {
    font = {
      name = "FiraCode Nerd Font Mono";
      size = "11";
    };
    keybindings = {
      "cmd+shift+c" = "copy_to_clipboard";
      "cmd+shift+v" = "paste";

      "cmd+c" = "send_text application \x03";
      "cmd+d" = "send_text application \x04";
      "cmd+e" = "send_text application \x05";
      "cmd+f" = "send_text application \x06";
      "cmd+g" = "send_text application \x07";
      "cmd+h" = "send_text application \x08";
      "cmd+i" = "send_text application \x09";
      "cmd+j" = "send_text application \x0A";
      "cmd+k" = "send_text application \x0B";
      "cmd+l" = "send_text application \x0C";
      "cmd+m" = "send_text application \x0D";
      "cmd+n" = "send_text application \x0E";
      "cmd+o" = "send_text application \x0F";
      "cmd+p" = "send_text application \x10";
      "cmd+q" = "send_text application \x11";
      "cmd+r" = "send_text application \x12";
      "cmd+s" = "send_text application \x13";
      "cmd+t" = "send_text application \x14";
      "cmd+u" = "send_text application \x15";
      "cmd+v" = "send_text application \x16";
      "cmd+w" = "send_text application \x17";
      "cmd+x" = "send_text application \x18";
      "cmd+y" = "send_text application \x19";
      "cmd+z" = "send_text application \x1A";
      "cmd+[" = "send_text application \x1B";
      "cmd+\\" = "send_text application \x1C";
      "cmd+]" = "send_text application \x1D";
      "cmd+^" = "send_text application \x1E";
      "cmd+_" = "send_text application \x1F";
    };

    settings = {
      "wayland_titlebar_color" = "background";
    };

    theme = "Gruvbox Dark";
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "Catppuccin-mocha";
    };
    themes = {
      catppuccin-mocha = builtins.readFile (pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
        sha256 = "6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
      } + "/Catppuccin-mocha.tmTheme");
    };
  };

  programs.skim = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      format = "$username$hostname$shlvl$directory$git_branch$git_commit$git_state$git_status$hg_branch$docker_context$package$cmake$dart$dotnet$elixir$elm$erlang$golang$helm$java$julia$kotlin$nim$nodejs$ocaml$perl$php$purescript$python$ruby$rust$swift$terraform$vagrant$zig$nix_shell$conda$memory_usage$aws$gcloud$openstack$env_var$crystal$kubernetes$custom$cmd_duration$line_break$lua$jobs$battery$time$status$character";
      aws = {
        style = "bold yellow";
        symbol = " ";
        format = "[$symbol($profile )(\\($region\\) )(\\[$duration\\] )]($style)";
        region_aliases = {
          us-east-1 = "va";
          us-east-2 = "oh";
          us-west-1 = "ca";
          us-west-2 = "or";
        };
      };
      cmd_duration = {
        min_time = 2000;
        show_notifications = true;
        style = "bold blue";
      };
      custom = {
        nomad = {
          command = "if [[ $NOMAD_ADDR =~ \".prod.\" ]]; then\n    echo \"Prod\"\nelif [[ $NOMAD_ADDR =~ \".staging.\" ]]; then\n    echo \"Staging\"\nelif [[ $NOMAD_ADDR =~ \"http://192.*\" ]]; then\n    echo \"Vagrant\"\nelif [[ $NOMAD_ADDR =~ \"http://127.*\" ]]; then\n    echo \"Local\"\nfi\n";
          format = "$symbol [$output]($style) ";
          shell = [
            "sh"
          ];
          symbol = "";
          when = " [[ -v NOMAD_TOKEN ]] && [[ -v NOMAD_ADDR ]] ";
        };
      };
      directory = {
        style = "bold blue";
        truncation_length = 5;
        truncation_symbol = "…/";
      };
      gcloud = {
        disabled = true;
      };
      git_status = {
        ahead = " \${count}";
        behind = " \${count}";
        conflicted = " ";
        deleted = " ";
        diverged = "⇕⇡\${ahead_count}⇣\${behind_count}";
        format = "([$all_status$ahead_behind]($style) )";
        modified = "[ ](blue)";
        renamed = "➜";
        staged = "[ \\($count\\)](green) ";
        stashed = "[ \\($count\\)](yellow) ";
        untracked = " ";
      };
      helm = {
        disabled = true;
      };
      kubernetes = {
        disabled = false;
        format = "[$symbol$context( \\($namespace\\))]($style) ";
        symbol = "ﴱ ";
      };
      nodejs = {
        disabled = true;
      };
      php = {
        disabled = true;
      };
      terraform = {
        disabled = true;
      };
      palette = "catppuccin_mocha";
    } // builtins.fromTOML (builtins.readFile
    (pkgs.fetchFromGitHub
    {
      owner = "catppuccin";
      repo = "starship";
      rev = "3e3e54410c3189053f4da7a7043261361a1ed1bc";
      sha256 = "soEBVlq3ULeiZFAdQYMRFuswIIhI9bclIU8WXjxd7oY=";
    } + /palettes/mocha.toml));
  };

  xdg.configFile."fish/themes/Catppuccin Mocha.theme" = {
    source = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/catppuccin/fish/0ce27b518e8ead555dec34dd8be3df5bd75cff8e/themes/Catppuccin%20Mocha.theme";
      sha256 = "MlI9Bg4z6uGWnuKQcZoSxPEsat9vfi5O1NkeYFaEb2I=";
    };
  };
  # Still need to figure out how to run ``fish_config theme save "Catppuccin Mocha"` once automatically without making a custom package


  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      fish_vi_key_bindings
      '';
    plugins = [
      { name = "grc"; src = pkgs.fishPlugins.grc.src; }
    ];
  };
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    shellAliases = {
      sudo = "sudo ";
      ls = "exa";
      bgrep = "batgrep";
      man = "batman";
      bw = "batwatch";
      pbat = "prettybat";
      cat = "bat --paging=never";
      k9s = "TERM=xterm-256color k9s";
    };
    history = {
      size = 1000000000; 
      path = "${config.xdg.dataHome}/zsh/history";
    };
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    completionInit = ''
    zstyle ':completion:*' completer _expand _complete _ignored _approximate
    zstyle ':completion:*' use-cache on
    zstyle ':completion:*' expand prefix suffix
    zstyle ':completion:*' file-sort access
    zstyle ':completion:*' ignore-parents parent .. directory
    zstyle ':completion:*' list-colors ""
    zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
    zstyle ':completion:*' list-suffixes true
    zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
    zstyle ':completion:*' menu select=1
    zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
    zstyle :compinstall filename ~/.config/zsh/.zshrc

    autoload -Uz compinit
    compinit
    '';

    initExtraFirst=''
    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    fi
    '';
    initExtra=''
    export BAT_THEME=Catppuccin-mocha
    PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig";
    export SKIM_DEFAULT_OPTIONS="$SKIM_DEFAULT_OPTIONS --color=fg:#cdd6f4,bg:#1e1e2e,matched:#313244,matched_bg:#f2cdcd,current:#cdd6f4,current_bg:#45475a,current_match:#1e1e2e,current_match_bg:#f5e0dc,spinner:#a6e3a1,info:#cba6f7,prompt:#89b4fa,cursor:#f38ba8,selected:#eba0ac,header:#94e2d5,border:#6c7086"
    source "${pkgs.zsh-defer}/share/zsh/plugins/zsh-defer/zsh-defer.plugin.zsh"
    zsh-defer source "${pkgs.grc}/etc/grc.zsh"
    autoload -Uz edit-command-line
    zle -N edit-command-line
    bindkey -M vicmd 'v' edit-command-line
    bindkey '^R' history-incremental-search-backward
    zsh-defer source ${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh
    . ~/.config/zsh/local.zsh
    alias bathelp="bat --plain --language=cmd-help";
    help() {
      set -o pipefail
      "$@" --help 2>&1 | bathelp
    }
    '';
  };

  xdg.configFile."zsh/.zshnew".text = ''
  fpath+=(${pkgs.zoxide}/share/zsh/site-functions)
  fpath+=(${pkgs.zsh-completions}/share/zsh/site-functions)
  fpath+=("${pkgs.kubectl}/share/zsh/site-functions/")
  fpath+=("${pkgs.trunk.kubernetes-helm}/share/zsh/site-functions/")
  fpath+=("${pkgs.bat}/share/zsh/site-functions/")
  fpath+=("${pkgs.exa}/share/zsh/site-functions/")
  fpath+=("${pkgs.fd}/share/zsh/site-functions/")

  zsh-defer source "${pkgs.skim}/share/skim/key-bindings.zsh"
  zsh-defer source "${pkgs.zsh-autopair}/share/zsh/zsh-autopair"
  zsh-defer source "${pkgs.zsh-you-should-use}/share/zsh/plugins/you-should-use/you-should-use.plugin.zsh"
  zsh-defer source "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh"
  zsh-defer source "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
  zsh-defer source "${pkgs.grc}/etc/grc.zsh"

  zsh-defer eval "$(zoxide init zsh)"
  '';

}
