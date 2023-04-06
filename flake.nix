{
  description = "m-wynn/dotfiles";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    trunk.url = "github:nixos/nixpkgs";
    # unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home = {
      url = "https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = inputs @ { self, home, nixpkgs, fenix, ... }:
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
        unstable = import inputs.nixpkgs { system = self.system; };
        stable = import inputs.nixpkgs { system = self.system; };
        trunk = import inputs.trunk { system = self.system; };
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
