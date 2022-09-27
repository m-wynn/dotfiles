{
  description = "m-wynn/dotfiles";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
    trunk.url = "github:nixos/nixpkgs";
    # unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home = {
      url = "https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz";
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
        system = "aarch64-darwin";
        username = "matthew";
        homeDirectory = "/Users/matthew";
        stateVersion = "22.05";
        configuration = { pkgs, imports, ... }:
          {
            imports = [
              ./home.nix
              ./shell.nix
            ];
            nixpkgs.overlays = overlays;
          };
      };

      wsl = home.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          ./home.nix
          {
            nixpkgs.overlays = overlays;
            home = {
              username = "matthew";
              homeDirectory = "/home/matthew";
              stateVersion = "22.05";
            };
          }
        ];
      };
    };
  };
}
