{ pkgs, fetchFromGitHub, stdenv }:
stdenv.mkDerivation rec {
  name = "pack-beam";
  buildInputs = with pkgs; [
    cmake
    gperf
    erlangR23
    elixir
    zlib
  ];
  src = fetchFromGitHub {
    owner = "atomvm";
    repo = "AtomVM";
    rev = "135de9eace87d62ead83d4f2a2f8842013626914";
    fetchSubmodules = true;
    sha256 = "QMokRAkdemdvyvwahl/nFjqgXxJzKhcYygligtrDEl8=";
  };

  configurePhase = ''
    mkdir -p $out/lib/build
    mv * $out/lib/
    cd $out/lib/build
    cmake -DAVM_DISABLE_FP=on $out/lib
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp $out/lib/build/tools/packbeam/PackBEAM $out/bin/PackBEAM
  '';
}
