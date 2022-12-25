{ pkgs, ...}:

let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix/";
    # place version number with the latest one from the github releases page
    ref = "refs/tags/3.5.0";
  }) {};
  #requirements-for-esp-idf = pkgs.fetchurl {
  #        url = https://github.com/espressif/esp-idf/tree/release/v4.2/requirements.txt;
  #        hash = "sha256-idbOzuZol39dT2HK62+uMM+uLVqYDBLjgpw7CVDG12I=";
  #      };
in
  mach-nix.mkPython {
    requirements = builtins.readFile ./requirements.txt;
  }
