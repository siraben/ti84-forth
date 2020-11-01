{ pkgs ? import <nixpkgs> {}}:
with pkgs;

let
  spasm-ng = callPackage ./spasm-ng.nix {};
in

stdenv.mkDerivation {
  name = "ti84-forth";
  src = ./.;
  nativeBuildInputs = [ spasm-ng ];

  buildPhase = ''
    spasm forth.asm forth.8xp
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv forth.8xp $out/bin
  '';
}
