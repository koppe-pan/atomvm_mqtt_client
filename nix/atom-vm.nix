{ pkgs, fetchFromGitHub, stdenv, lib }:
let
  esp-idf = (pkgs.callPackage ./esp-idf.nix {});
  python_env = pkgs.callPackage ./python-env.nix {};
  xtensa = pkgs.callPackage ./xtensa-esp-32-elf.nix {};
  atomvm_mqtt_client = fetchFromGitHub {
    owner = "atomvm";
    repo = "atomvm_mqtt_client";
    rev = "1b12cb079eb25ac48b196fa8b6e13ece8fa0ebdb";
    sha256 = "jjtpteX6Dw8PNe5ws9IQhoeA+zY/yl4f7OI2n168tfg=";
  };
  rc522 = fetchFromGitHub {
    owner = "abobija";
    repo = "esp-idf-rc522";
    rev = "a680175675256f192e0de943a4170f4ccabe877d";
    sha256 = "i6USspAfHAwnu467LG0WbkrFiyuiXacXWFfPV0+tE6A=";
  };
  atomvm_rfid = fetchFromGitHub {
    owner = "koppe-pan";
    repo = "atomvm_rfid";
    rev = "6e1c74f3e0f6e6efc3304b037862a79f6be5f282";
    sha256 = "Lk1w/SyUofkUYbZXz7Qk7VV8vUn/2tlTAoM3RJ9gV6Y=";
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
  src = pkgs.callPackage ./pack-beam.nix {};

  preUnpack = esp-idf.shellHook;

  configurePhase = ''
    mkdir -p $out/lib/build
    mv lib/* $out/lib/

    mkdir -p $out/lib/src/platforms/esp32/components/atomvm_mqtt_client
    cp -avr ${atomvm_mqtt_client}/* $out/lib/src/platforms/esp32/components/atomvm_mqtt_client/

    #mkdir -p $out/lib/src/platforms/esp32/components/rc522
    #rm -rf $out/lib/src/platforms/esp32/components/rc522/*
    #cp -avr ${../../rc522}/* $out/lib/src/platforms/esp32/components/rc522/
    #cp -avr ${rc522}/* $out/lib/src/platforms/esp32/components/rc522/

    #mkdir -p $out/lib/src/platforms/esp32/components/atomvm_rfid
    #cp -avr ${../../atomvm_rfid}/* $out/lib/src/platforms/esp32/components/atomvm_rfid/
    #cp -avr ${atomvm_rfid}/* $out/lib/src/platforms/esp32/components/atomvm_rfid/
  '';

  buildPhase = ''
    cd $out/lib/src/platforms/esp32
    idf.py set-target esp32
    idf.py build
  '';

  installPhase = ''
    mkdir -p $out/bin
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
