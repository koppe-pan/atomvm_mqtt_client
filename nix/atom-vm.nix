{ pkgs, fetchFromGitHub, stdenv, lib }:
let
  esp-idf = (pkgs.callPackage ./esp-idf.nix {});
  python_env = pkgs.callPackage ./python-env.nix {};
  xtensa = pkgs.callPackage ./xtensa-esp-32-elf.nix {};
  esp_mqtt = fetchFromGitHub {
    owner = "espressif";
    repo = "esp-mqtt";
    rev = "985078affa8a2d2b56b87c8e6455252850f895c6";
    sha256 = "CSFmCRYQzCXsyO0JpRZSxM/IqcBhBJPBFEG4zNVXtjM=";
  };
  atomvm_mqtt_client = fetchFromGitHub {
    owner = "atomvm";
    repo = "atomvm_mqtt_client";
    rev = "1b12cb079eb25ac48b196fa8b6e13ece8fa0ebdb";
    sha256 = "jjtpteX6Dw8PNe5ws9IQhoeA+zY/yl4f7OI2n168tfg=";
  };
in
stdenv.mkDerivation rec {
  name = "atom-vm";
  nativeBuildInputs = [ pkgs.makeWrapper ];

  buildInputs = with pkgs; [
    python_env
    esp-idf
    xtensa
    cmake
    gperf
    erlangR23
    elixir
    zlib
    git
    which
  ];

  src = fetchFromGitHub {
    owner = "atomvm";
    repo = "AtomVM";
    rev = "135de9eace87d62ead83d4f2a2f8842013626914";
    fetchSubmodules = true;
    sha256 = "QMokRAkdemdvyvwahl/nFjqgXxJzKhcYygligtrDEl8=";
  };

  preUnpack = esp-idf.shellHook;

  configurePhase = ''
    mkdir -p $out/lib/build
    mv * $out/lib/

    mkdir -p $out/lib/src/platforms/esp32/components/mqtt
    cp -avr ${esp_mqtt}/* $out/lib/src/platforms/esp32/components/mqtt/

    mkdir -p $out/lib/src/platforms/esp32/components/atomvm_mqtt_client
    cp -avr ${atomvm_mqtt_client}/* $out/lib/src/platforms/esp32/components/atomvm_mqtt_client/

    cd $out/lib/build
    cmake -DAVM_DISABLE_FP=on $out/lib
  '';

  buildPhase = ''
    make
    cd $out/lib/src/platforms/esp32
    idf.py set-target esp32
    idf.py build
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp $out/lib/build/tools/packbeam/PackBEAM $out/bin/PackBEAM


    cat <<!ENDOFWFILTER! > $out/bin/flash
    #!${pkgs.bash}/bin/bash
    #
    # This file is part of AtomVM.
    #
    # Copyright 2020 Fred Dushin <fred@dushin.net>
    #
    # Licensed under the Apache License, Version 2.0 (the "License");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at
    #
    #    http://www.apache.org/licenses/LICENSE-2.0
    #
    # Unless required by applicable law or agreed to in writing, software
    # distributed under the License is distributed on an "AS IS" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and
    # limitations under the License.
    #
    # SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
    #

    ${esp-idf.shellHook}
    IMG_FILE="\$1/atomvm-esp32-0.6.0-dev.img"
    escript $out/lib/src/platforms/esp32/build/mkimage.erl \
    --root_dir $out/lib \
    --config $out/lib/src/platforms/esp32/build/mkimage.config \
    --out \$IMG_FILE \
    "$@"

    echo "============================================="
    echo ""
    echo "AtomVM esp32 version 0.6.0-dev image written to:"
    echo \$IMG_FILE

    FLASH_OFFSET="0x1000" $out/lib/src/platforms/esp32/build/flash.sh \$IMG_FILE
    !ENDOFWFILTER!
    chmod 755 $out/bin/flash
    wrapProgram $out/bin/flash \
    --prefix PATH : "${lib.makeBinPath buildInputs}"


    cat <<!ENDOFWFILTER! > $out/bin/run
    #!${pkgs.bash}/bin/bash

    esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x210000 \$1
    !ENDOFWFILTER!
    chmod 755 $out/bin/run
    wrapProgram $out/bin/run \
    --prefix PATH : "${lib.makeBinPath buildInputs}"


    cat <<!ENDOFWFILTER! > $out/bin/monitor
    #!${pkgs.bash}/bin/bash

    cd $out/lib/src/platforms/esp32
    idf.py monitor
    cd -
    !ENDOFWFILTER!
    chmod 755 $out/bin/monitor
    wrapProgram $out/bin/monitor \
    --prefix PATH : "${lib.makeBinPath buildInputs}"
  '';
}
