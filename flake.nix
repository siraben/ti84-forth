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
            spasm -L -T forth.asm forth.8xp
            data_start_hex=$(awk '$1 == "DATA_START" { sub(/^\$/, "", $3); print $3 }' forth.lab)
            data_end_hex=$(awk '$1 == "DATA_END" { sub(/^\$/, "", $3); print $3 }' forth.lab)
            test -n "$data_start_hex"
            test -n "$data_end_hex"
            test "$((16#$data_end_hex))" -lt "$((16#C000))"
            test "$((16#$data_end_hex - 16#$data_start_hex))" -eq 354
          '';
        
          installPhase = ''
            mkdir -p $out
            mv forth.8xp $out/
          '';
        };

        devShell = mkShell {
          packages = [ python3 spasm-ng ];
        };
      }
    );
}
