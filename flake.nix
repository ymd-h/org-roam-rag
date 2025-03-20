{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        lib.mkPackage = ({ epkgs }: (epkgs.trivialBuild rec {
          pname = "org-roam-rag";
          version = "v0.0.1";
          src = ./.;
          packageRequires = with epkgs; [
            llm
            markdown-mode
            org-roam
          ];
          buildInputs = packageRequires;
        }));

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.ollama
            pkgs.duckdb
            ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [
              (lib.mkPackage { inherit epkgs; })
            ]))
          ];
        };
      }
    );
}
