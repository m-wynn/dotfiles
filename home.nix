{ config, pkgs, ... }:
let
in
  {
    programs.home-manager.enable = true;
    home.packages = with pkgs; [
      trunk.wezterm
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

      bat-extras.batdiff
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.batwatch
      bat-extras.prettybat
      trunk.awscli2
      aws-sam-cli
      trunk.ssm-session-manager-plugin
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
      trunk.kustomize
      kubelogin-oidc
      mysql
      trunk.kubernetes-helm
      ripgrep
      sad
      sqlite
      starship
      trunk.terraform
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
      trunk.terraform-ls
      terraform-lsp
      tflint
      zlib
      google-cloud-sdk
      pkg-config
      trunk.nodePackages.yaml-language-server
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
      package = pkgs.trunk.neovim-unwrapped;
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
