{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  home.packages = [
    pkgs.neovim-nightly

    pkgs.grc
    pkgs.starship
    pkgs.bat
    pkgs.exa
    pkgs.fd
    pkgs.htop
    pkgs.ripgrep
    pkgs.skim
    pkgs.tree
    pkgs.gh

    pkgs.tmux

    pkgs.ctags
    pkgs.deno
    pkgs.pyright
    pkgs.rnix-lsp
    pkgs.terraform-ls

    pkgs.rustup
    pkgs.rust-analyzer-nightly
  ];
}
