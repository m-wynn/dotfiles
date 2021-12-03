# Setup

## Install nix

```bash
sh <(curl -L https://nixos.org/nix/install)
. ~/.nix-profile/etc/profile.d/nix.sh
```

## Enable Flakes

```bash
nix-env -iA nixpkgs.nixFlakes
??? > /etc/nix/nix.conf "experimental-features = nix-command flakes"
```

## Add cachix

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cahix use nix-community
```

## Install home-manager

```bash
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
```
