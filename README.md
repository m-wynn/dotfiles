# Setup

## Place this repository in ~/.config/nixpkgs

```bash
git clone git@github.com:m-wynn/dotfiles.git ~/.config/nixpkgs
```
or 
```bash
rm -r ~/.config/nixpkgs
ln -s ~/dotfiles ~/.confix/nixpkgs
```

## Install nix

```bash
sh <(curl -L https://nixos.org/nix/install)
. ~/.nix-profile/etc/profile.d/nix.sh
```

## Enable Flakes

```bash
nix-env -iA nixpkgs.nixFlakes
echo "experimental-features = nix-command flakes" | sudo tee /etc/nix/nix.conf 
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

## Switch to the proper home-manager config

E.g. for the WSL profile:

```bash
 home-manager switch --flake ~/.config/nixpkgs/\#wsl -v

```

