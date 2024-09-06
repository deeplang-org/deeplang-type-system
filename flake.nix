{
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
    in {
      devShells."x86_64-linux".default = pkgs.mkShell {
        buildInputs = with pkgs.ocamlPackages; [
          ocaml dune_3 findlib odoc ppx_deriving ppxlib
        ];
      };
    };
}

