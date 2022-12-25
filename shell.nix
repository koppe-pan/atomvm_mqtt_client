{ pkgs ? import <nixpkgs> {} }:

let
  esp-idf = (pkgs.callPackage ./nix/esp-idf.nix {});
  atom-vm = (pkgs.callPackage ./nix/atom-vm.nix {});
in
pkgs.mkShell {
  name = "esp-idf-env";
  buildInputs = with pkgs; [
    atom-vm
    esp-idf
    rebar3
  ];
}
