{ pkgs ? import <nixpkgs> {} }:

let
  esp-idf = (pkgs.callPackage ./nix/esp-idf.nix {});
  atom-vm = (pkgs.callPackage ./nix/atom-vm.nix {});
  pack-beam = (pkgs.callPackage ./nix/pack-beam.nix {});
in
pkgs.mkShell {
  name = "esp-idf-env";
  buildInputs = with pkgs; [
    atom-vm
    esp-idf
    pack-beam
    rebar3
  ];
}
