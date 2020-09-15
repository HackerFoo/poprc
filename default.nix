{ pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation {
  name = "poprc";
  src = ./.;
  buildInputs = [ git readline clang ];
  buildFlags = [ "PREFIX=$(out)" "EXTRA_CFLAGS=$(NIX_CFLAGS_COMPILE)" "USE_READLINE=y" ];
  installFlags = [ "PREFIX=$(out)" ];
}
