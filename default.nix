{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let name = "poprc";
    localSrc = ./.;
    gitSrc = fetchGit {
      inherit name;
      url = https://github.com/HackerFoo/poprc.git;
      rev = "<revision>";
    };
in stdenv.mkDerivation rec {
  inherit name;
  src = localSrc;
  buildInputs = [ readline ];
  buildFlags = [ "PREFIX=$(out) EXTRA_CFLAGS=$(NIX_CFLAGS_COMPILE) USE_READLINE=y" ];
  installFlags = [ "PREFIX=$(out)" ];
}
