{ config, pkgs, ... }:
let
  python-with-packages = pkgs.python3.withPackages (pp: with pp; [

  ]);

in
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
    pkgs.ripgrep
    pkgs.skim
    pkgs.sqlite
    pkgs.terraform
    pkgs.tmux

    # pkgs.python39
    python-with-packages

    # linters and checkers
    pkgs.checkov
    pkgs.ctags
    pkgs.deno
    pkgs.pyright
    pkgs.rnix-lsp
    pkgs.terraform-ls
    pkgs.openssl
    pkgs.pkg-config
    pkgs.sumneko-lua-language-server

    pkgs.nodePackages.eslint_d
    pkgs.nodePackages.fixjson

    # Compilers
    pkgs.rustup
    pkgs.libstdcxx5
    pkgs.gcc

  ];

  home.sessionVariables = {
    PYTHONPATH = "${python-with-packages}/${python-with-packages.sitePackages}";
  };
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
    extraPython3Packages = (ps: with ps; [
      msgpack
      pynvim
    ]);
    extraPackages = [
      pkgs.sqlite
      pkgs.tree-sitter
    ];
    plugins =  with pkgs.vimPlugins; [
      yankring
      vim-nix
      { plugin = sqlite-lua; }
    ];
    extraConfig = builtins.concatStringsSep "\n" [
      ''
      luafile ${builtins.toString /home/matthew/.config/nvim/init_lua.lua}
      ''
    ];
  };
  xdg.configFile."nvim" = {
    source = ./.config/nvim;
    recursive = true;
  };
  xdg.configFile."zsh" = {
    source = ./.config/zsh;
    recursive = true;
  };
}
