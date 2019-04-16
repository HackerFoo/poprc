{ pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation rec {
  name = "poprc";
  src = fetchGit {
    name = "poprc";
    url = https://github.com/HackerFoo/poprc.git;
    rev = "<revision>";
  };
  installFlags = [ "PREFIX=$(out)" ];
}
