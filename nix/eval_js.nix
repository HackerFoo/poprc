# Run `make node2nix` to generate `node` directory.

{ pkgs ? import <nixpkgs> { } }:

with pkgs;
with (import ./node { inherit pkgs; });

stdenv.mkDerivation {
  name = "poprc_eval_js";
  src = ./..;
  buildInputs = [ git emscripten ];
  phases = "unpackPhase buildPhase installPhase";
  buildPhase = ''
    export HOME=$PWD
    git clean -fxd
    make .gen/git_log.h
    sed -i 's/ \[DIRTY\]"$/"/' .gen/git_log.h*
    make js/eval.js BUILD=release-with-asserts
  '';
  installPhase = ''
    mkdir $out
    cp -r js css eval.html $out
    cp -r ${package}/lib/node_modules/PoprC/node_modules $out
  '';
}
