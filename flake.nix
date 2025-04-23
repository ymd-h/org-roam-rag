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
          version = "v1.0.0";
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./org-roam-rag.el
            ];
          };
          packageRequires = with epkgs; [
            llm
            markdown-mode
            org-roam
            ox-gfm
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
