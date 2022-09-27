{ lib, stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "zsh-defer";

  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "romkatv";
    repo = "zsh-defer";
    rev = "57a6650ff262f577278275ddf11139673e01e471";
    sha256 = "sha256-/rcIS2AbTyGw2HjsLPkHtt50c2CrtAFDnLuV5wsHcLc=";
  };

  dontBuild = true;

  installPhase = ''
    install -D zsh-defer.plugin.zsh $out/share/zsh/plugins/zsh-defer/zsh-defer.plugin.zsh
  '';

  meta = with lib; {
    homepage = "https://github.com/romkatv/zsh-defer";
    license = licenses.gpl3;
    description = "Deferred execution of Zsh commands";
    maintainers = with maintainers; [ dinoocch ];
  };
}

