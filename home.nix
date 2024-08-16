{ pkgs, ... }:
  {
    programs.home-manager.enable = true;
    home.packages = with pkgs; [
      trunk.wezterm # https://nixpk.gs/pr-tracker.html?pr=334814
      fira-code
      discocss
      comma
      dive
      libiconvReal

      prometheus-alertmanager

      prometheus

      zsh-defer
      zsh-you-should-use
      zsh-fast-syntax-highlighting
      zsh-completions
      zsh-autosuggestions
      terraform-zsh-plugin
      grc
      zoxide
      krew

      fzf

      stable.bat-extras.batdiff # https://nixpk.gs/pr-tracker.html?pr=334814
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.batwatch
      bat-extras.prettybat
      stable.aws-sam-cli # https://github.com/NixOS/nixpkgs/issues/325990
      ssm-session-manager-plugin
      dog
      eza
      fd
      gh
      go
      gopls
      hadolint
      htop
      json2hcl
      lazygit
      kubectl
      kustomize
      kubelogin-oidc
      mysql
      kubernetes-helm
      ripgrep
      sad
      sqlite
      starship
      terraform
      tmux
      yamllint
      # php82Packages.phpstan
      # php82Packages.phpcbf
      # php82Packages.php-cs-fixer

      (python3.withPackages (pp: with pp; [
        boto3
        botocore
        cryptography
        isort
        psutil
        pip
        pyyaml
        flake8
        black
      ]))

      argocd
      # checkov
      ctags
      deno
      pyright
      shellharden
      terraform-ls
      terraform-lsp
      tflint
      zlib
      google-cloud-sdk
      pkg-config
      nodePackages.yaml-language-server
      npm-check-updates

      nodejs
      nodePackages.eslint_d
      nodePackages.fixjson

      rustup
      gcc
      cmake
      argo-rollouts
    ];

    home.sessionVariables = {
      EDITOR = "nvim";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      KUBECTL_EXTERNAL_DIFF = "delta";
    };
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      vimAlias = true;
      vimdiffAlias = true;
      withPython3 = true;
      extraPython3Packages = (ps: with ps; [
        msgpack
        pyyaml
        python-dotenv
        requests
        prompt-toolkit
      ]);
      extraPackages = [
        # pkgs.stable.checkov
        pkgs.sqlite
        pkgs.tree-sitter
        pkgs.nil # I am having trouble compiling this inside Mason
      ];
      plugins =  with pkgs.vimPlugins; [
        yankring
        vim-nix
        { plugin = sqlite-lua; }
      ];
      extraConfig = builtins.concatStringsSep "\n" [
        ''
          luafile ${builtins.toString ~/.config/nvim/init_lua.lua}
        ''
      ];
    };
    xdg.configFile."nvim" = {
      source = ./config/nvim;
      recursive = true;
    };
  }
