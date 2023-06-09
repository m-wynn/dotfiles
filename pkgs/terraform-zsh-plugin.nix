{ lib, stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "terraform-zsh-plugin";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "jsporna";
    repo = pname;
    rev = "f81bad8e0197ae6c979f34174ec6dd18821867b7";
    sha256 = "sha256-NI0evIfgfu7cDB2+ZAzOT3wsC9V0PPqLZ0SxqN4TnPU=";
  };

  strictDeps = true;
  installPhase = ''
    install -D --target-directory=$out/share/zsh/site-functions _terraform
  '';

  meta = {
    description = "ZSH plugin to make work with terraform easier.";
    homepage = "https://github.com/jsporna/terraform-zsh-plugin/";
    license = lib.licenses.free;

    platforms = lib.platforms.unix;
    maintainers = [ ];
  };
}
