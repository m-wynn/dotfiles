{
  description = "m-wynn/dotfiles";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    trunk.url = "github:nixos/nixpkgs";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    catppuccin.url = "github:catppuccin/nix";

    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = inputs @ { self, home, nixpkgs, catppuccin, ... }:
  let
    nixpkgs_config = {
      allowUnfree = true;
      allowBroken = true;
    };
    nixpkgs-stable_config = {
      allowUnfree = true;
    };
    overlays = [
      # Inject 'unstable' and 'trunk' into the overridden package set, so that
      # the following overlays may access them (along with any system configs
      # that wish to do so).
      (self: super: {
        # fcitx-engines = self.fcitx5;
        zsh-defer = super.callPackage ./pkgs/zsh-defer.nix { };
        terraform-zsh-plugin = super.callPackage ./pkgs/terraform-zsh-plugin.nix { };
        unstable = import inputs.unstable { system = self.system; };
        stable = import inputs.nixpkgs { system = self.system; };
        trunk = import inputs.trunk { system = self.system; config = nixpkgs_config; };
      })
    ];


  in {
    homeConfigurations = {
      matthew = home.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        modules = [
          ./home.nix
          ./shell.nix
          {
            imports = [
              catppuccin.homeManagerModules.catppuccin
            ];
            catppuccin = {
              enable = true;
              accent = "lavender";
              flavor = "mocha";
            };
            home = {
              username = "matthew";
              homeDirectory = "/Users/matthew";
              stateVersion = "22.05";
            };
            nixpkgs.overlays = overlays;
          }
        ];
      };

      wsl = home.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          ./home.nix
          ./shell.nix
          {
            home = {
              username = "matthew";
              homeDirectory = "/home/matthew";
              stateVersion = "22.05";
            };
            nixpkgs.overlays = overlays;
          }
        ];
      };
    };
  };
}
