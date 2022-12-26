{ pkgs, fetchFromGitHub, stdenv, lib }:
let
  python_env = pkgs.callPackage ./python-env.nix {};
in
stdenv.mkDerivation rec {
  name = "esp-idf";
  nativeBuildInputs = [ pkgs.makeWrapper ];

  buildInputs = with pkgs; [
    ninja
    cmake
    ccache
    dfu-util
  ];

  src = fetchFromGitHub {
    owner = "espressif";
    repo = "esp-idf";
    rev = "v4.2";
    fetchSubmodules = true;
    sha256 = "0h60xnxny0q4m3mqa1ghr2144j0cn6wb7mg3nyn31im3dwclf68h";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    makeWrapper ${python_env}/bin/python $out/bin/idf.py \
    --add-flags ${src}/tools/idf.py \
    --set IDF_PATH ${src} \
    --prefix PATH : "${lib.makeBinPath buildInputs}"
    makeWrapper ${python_env}/bin/python $out/bin/esptool.py \
    --add-flags ${src}/components/esptool_py/esptool/esptool.py \
    --set IDF_PATH ${src} \
    --prefix PATH : "${lib.makeBinPath buildInputs}"
  '';

  shellHook = ''
    export IDF_PATH=${src}
    export IDF_PYTHON_ENV_PATH=${python_env}/bin/python
  '';
}
