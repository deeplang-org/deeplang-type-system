with import <nixpkgs> {};

mkShell {
    buildInputs = with ocamlPackages; [
        ocaml dune_3 findlib odoc ppx_deriving ppxlib
    ];
}
