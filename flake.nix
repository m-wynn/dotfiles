{
  description = "m-wynn/dotfiles";
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, home, nixpkgs, fenix, ... }:
  let
    nixpkgs_config = {
      allowUnfree = true;
    };

    overlays = [
      # inputs.neovim-nightly-overlay.overlay
      # fenix.overlay
      # (self: super:
      #   {
      #     zsh-defer = super.callPackage ./pkgs/zsh-defer.nix { };
      #   }
      # )
    ];

  in {
    homeConfigurations = {
      work = home.lib.homeManagerConfiguration {
        system = "amd64-darwin";
        username = "m-wynn";
        homeDirectory = "/Users/m-wynn";
        configuration = { pkgs, imports, ... }:
          {
            imports = [ ./home.nix ];
            nixpkgs.overlays = overlays;
          };
      };

      wsl = home.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        username = "matthew";
        homeDirectory = "/home/matthew";
        configuration = { pkgs, imports, ... }:
          {
            imports = [ ./home.nix ];
            nixpkgs.overlays = overlays;
          };
      };
    };
  };
}
