{ config, pkgs, ... }:
{
  xdg = {
    enable = true;
  };
  programs.awscli = {
    enable = true;
    package = pkgs.awscli2;
  };
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
    ];
    terminal = "tmux-256color";
    extraConfig = ''
    set-option -g -q mouse on
    set-option -ga terminal-overrides ",xterm-256color:RGB"
    set-option -sg escape-time 10
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

  programs.bat = {
    enable = true;
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
        expiration_symbol = "";
        format = "[$symbol($profile )(\\($region\\) )]($style)";
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
        format = "[$symbol$context]($style) ";
        symbol = "ﴱ ";
        contexts = [
          { context_pattern = "prod"; style = "red"; }
        ];
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
    };
  };

  programs.k9s = {
    enable = true;
    aliases = {
      aliases = {
        tgb = "elbv2.k8s.aws/v1beta1/targetgroupbindings";
        dp = "deployments";
        sec = "v1/secrets";
        jo = "jobs";
        cr = "clusterroles";
        crb = "clusterrolebindings";
        ro = "roles";
        rb = "rolebindings";
        np = "networkpolicies";
      };
    };
    views = {
      views = {
        "v1/pods" = {
          columns = [
            "AGE"
            "NAMESPACE"
            "NAME"
            "IP"
            "NODE"
            "STATUS"
            "READY"
          ];
        };
      };
    };
  };
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    shellAliases = {
      sudo = "sudo ";
      ls = "eza";
      k = "kubectl";
      g = "git";
      bgrep = "batgrep";
      man = "batman";
      bw = "batwatch";
      pbat = "prettybat";
      cat = "bat --paging=never";
      k9s = "TERM=xterm-256color k9s";
      argocopy = "KUBECTL_EXTERNAL_DIFF=\"diff -u\" argocd";
      kopy = "KUBECTL_EXTERNAL_DIFF=\"diff -u\" kubectl";
      kust = "kustomize --enable-helm --load-restrictor=LoadRestrictionsNone build";
      cdr = "cd $(git root)";
    };
    history = {
      size = 1000000000; 
      path = "${config.xdg.dataHome}/zsh/history";
    };
    defaultKeymap = "viins";
    autosuggestion = {
      enable = true;
    };
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
    export XDG_CONFIG_HOME="${config.xdg.configHome}"
    export XDG_DATA_HOME="${config.xdg.dataHome}"
    export XDG_CACHE_HOME="${config.xdg.cacheHome}"
    export XDG_STATE_HOME="${config.xdg.stateHome}"
    PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig";
    export SKIM_DEFAULT_OPTIONS="$SKIM_DEFAULT_OPTIONS --color=fg:#cdd6f4,bg:#1e1e2e,matched:#313244,matched_bg:#f2cdcd,current:#cdd6f4,current_bg:#45475a,current_match:#1e1e2e,current_match_bg:#f5e0dc,spinner:#a6e3a1,info:#cba6f7,prompt:#89b4fa,cursor:#f38ba8,selected:#eba0ac,header:#94e2d5,border:#6c7086"
    source "${pkgs.zsh-defer}/share/zsh/plugins/zsh-defer/zsh-defer.plugin.zsh"
    zsh-defer source "${pkgs.grc}/etc/grc.zsh"
    autoload -Uz edit-command-line
    zle -N edit-command-line
    bindkey -M vicmd 'v' edit-command-line
    bindkey '^R' history-incremental-search-backward
    zsh-defer source ${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh
    zsh-defer source "${pkgs.awscli2}/share/zsh/site-functions/_aws" # This one is weird.  I think it's a nix bug that it's placed here
    . ~/.config/zsh/local.zsh
    alias bathelp="bat --plain --language=cmd-help";
    help() {
      set -o pipefail
      "$@" --help 2>&1 | bathelp
    }
    '';
  };

  programs.git = {
    enable = true;

    aliases = {
      amend = "commit --amend";
      br = "branch";
      cm = "commit";
      cmm = "commit --no-edit";
      co = "checkout";
      cob = "checkout -b";
      com = "!f(){ git checkout $(git main-branch) $@;}; f";
      d = "difftool";
      fe = "fetch --all -p --tags";
      fixup = "commit --amend -C HEAD";
      fpush = "push --force-with-lease";
      fu = "fetch upstream";
      lg = "log --color --graph --pretty=colorful-oneline --abbrev-commit";
      lga = "log --color --graph --pretty=colorful-oneline --abbrev-commit --all";
      ll = "!git log --color --graph --pretty=colorful-oneline --abbrev-commit --all --since=\"$(git show -s --pretty=format:'%cd' master~3 2>/dev/null || git log --format=\"format:%cd\" --reverse | head -n 1)\"";
      main-branch = "!git symbolic-ref refs/remotes/origin/HEAD | cut -d'/' -f4";
      pop = "stash pop";
      rev = "diff --staged -M";
      review = "diff --staged";
      root = "rev-parse --show-toplevel";
      save = "commit -m'savepoint'";
      st = "status -sb";
    };
    delta = {
      enable = true;
      package = pkgs.delta;
    };
    ignores = [
      "**/modules/*/.terraform.lock.hcl"
      ".venv"
      "venv"
    ];
    lfs = {
      enable = true;
    };
    includes = [
      { path = "~/.config/git/override"; }
    ];
    extraConfig = {
      rerere = {
        enabled = true;
        autoUpdate = true;
      };
      column = {
        ui = "auto";
      };
      branch = {
        sort = "-committerdate";
      };
      pretty = {
        colorful-oneline = "format:%C(yellow)%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset";
      };
      pull = {
        ff = "only";
      };
      push = {
        default = "current";
      };
      commit = {
        gpgsign = true;
      };
      gpg = {
        format = "ssh";
        signByDefault = true;
        ssh = {
          allowedSignersFile = "~/.ssh/allowed_signers";
        };
      };
      user = {
        signingKey = "~/.ssh/id_ed25519.pub";
      };
    };
    userEmail = "matthew@matthewwynn.com"; # Make dynamic?
    userName = "Matthew Wynn";
  };


  xdg.configFile."zsh/.zshnew".text = ''

  zsh-defer source "${pkgs.skim}/share/skim/key-bindings.zsh"
  zsh-defer source "${pkgs.zsh-autopair}/share/zsh/zsh-autopair"
  zsh-defer source "${pkgs.zsh-you-should-use}/share/zsh/plugins/you-should-use/you-should-use.plugin.zsh"
  zsh-defer source "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh"
  zsh-defer source "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
  zsh-defer source "${pkgs.grc}/etc/grc.zsh"

  zsh-defer eval "$(zoxide init zsh)"
  '';

}
