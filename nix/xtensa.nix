{ pkgs, stdenv, ...}:
stdenv.mkDerivation rec {
  name = "xtensa";
  src = pkgs.fetchurl {
    url = "https://github.com/espressif/crosstool-NG/releases/download/esp-2021r2-patch5/xtensa-esp32-elf-gcc8_4_0-esp-2021r2-patch5-linux-amd64.tar.gz";
    hash = "sha256-jvFOBAnCARtB5QSjD3DT41KHMTp5XR8kYq0s0OIFLTc=";
  };

  dontConfigure = true;
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/include
    mkdir -p $out/lib
    mkdir -p $out/libexec
    mkdir -p $out/xtensa-esp32-elf

    cp -avr * $out/
  '';
}
