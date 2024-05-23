{
  description = "A Forth interpreter for the TI-84 Plus calculators";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; {
        defaultPackage = stdenv.mkDerivation {
          pname = "ti84-forth";
          version = "head";
          src = ./.;
          nativeBuildInputs = [ spasm-ng ];
          buildPhase = ''
            spasm forth.asm forth.8xp
          '';
        
          installPhase = ''
            mkdir -p $out
            mv forth.8xp $out/
          '';
        };
      }
    );
}
