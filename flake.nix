{
  description = "m-wynn/dotfiles";
  inputs = {

    stable.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    trunk.url = "github:nixos/nixpkgs";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    catppuccin.url = "github:catppuccin/nix";

    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
  };

  outputs = inputs @ { self, home, nixpkgs, catppuccin, ... }:
  let
    nixpkgs_config = {
      allowUnfree = true;
      allowBroken = true;
    };
    overlays = [
      (self: super: {
        zsh-defer = super.callPackage ./pkgs/zsh-defer.nix { };
        terraform-zsh-plugin = super.callPackage ./pkgs/terraform-zsh-plugin.nix { };
        unstable = import inputs.unstable { system = self.system; config = nixpkgs_config; };
        stable = import inputs.stable { system = self.system; };
        trunk = import inputs.trunk { system = self.system; config = nixpkgs_config; };
      })
    ];


  in {
    homeConfigurations = {
      matthew = home.lib.homeManagerConfiguration {
        pkgs =  import nixpkgs { system = "aarch64-darwin"; config.allowUnfree = true; };
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
