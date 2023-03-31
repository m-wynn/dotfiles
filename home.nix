{ config, pkgs, ... }:
let
in
  {
    programs.home-manager.enable = true;
    home.packages = with pkgs; [
      kitty

      prometheus-alertmanager

      prometheus

      zsh-defer
      zsh-you-should-use
      zsh-fast-syntax-highlighting
      zsh-completions
      zsh-autosuggestions
      grc
      zoxide
      nomad
      krew

      fzf

      bat-extras.batdiff
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.batwatch
      bat-extras.prettybat
      trunk.awscli2
      bat
      delta
      dog
      exa
      fd
      gh
      go
      gopls
      htop
      json2hcl
      k9s
      kubectl
      kubelogin-oidc
      mysql
      trunk.kubernetes-helm
      ripgrep
      sad
      sqlite
      terraform
      tmux
      yamllint
      php82Packages.phpstan

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
      poetry

      checkov
      ctags
      deno
      pyright
      rnix-lsp
      shellharden
      trunk.terraform-ls
      terraform-lsp
      tflint
      openssl
      pkg-config
      sumneko-lua-language-server
      trunk.nodePackages.yaml-language-server

      nodePackages.eslint_d
      nodePackages.fixjson

      rustup
      gcc
      cmake
    ];

    home.sessionVariables = {
      EDITOR = "nvim";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
    };
    programs.neovim = {
      enable = true;
      # package = pkgs.trunk.neovim-unwrapped;
      vimAlias = true;
      vimdiffAlias = true;
      withPython3 = true;
      extraPython3Packages = (ps: with ps; [
        msgpack
        pynvim
        pyyaml
      ]);
      extraPackages = [
        pkgs.checkov
        pkgs.sqlite
        pkgs.tree-sitter
        pkgs.pyright
      ];
      plugins =  with pkgs.vimPlugins; [
        yankring
        vim-nix
        { plugin = sqlite-lua; }
      ];
      extraConfig = builtins.concatStringsSep "\n" [
        ''
          luafile ${builtins.toString /Users/matthew/.config/nvim/init_lua.lua}
        ''
      ];
    };
    xdg.configFile."nvim" = {
      source = ./.config/nvim;
      recursive = true;
    };
  }
