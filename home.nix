{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  home.packages = [

    # shell
    pkgs.zoxide
    pkgs.starship
    pkgs.grc

    # shell programs
    pkgs.bat
    pkgs.delta
    pkgs.exa
    pkgs.fd
    pkgs.gh
    pkgs.helm
    pkgs.htop
    pkgs.k9s
    pkgs.kubectl
    pkgs.neovim
    pkgs.ripgrep
    pkgs.skim
    pkgs.sqlite
    pkgs.terraform
    pkgs.tmux

    pkgs.python39
    pkgs.python39Packages.pynvim

    # linters and checkers
    pkgs.checkov
    pkgs.ctags
    pkgs.deno
    pkgs.pyright
    pkgs.rnix-lsp
    pkgs.terraform-ls
    pkgs.openssl
    pkgs.pkg-config

    pkgs.nodePackages.eslint_d
    pkgs.nodePackages.fixjson

    # Compilers
    pkgs.rustup
    pkgs.libstdcxx5
    pkgs.gcc
  ];
}
