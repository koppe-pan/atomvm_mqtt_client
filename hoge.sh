rm -rf _build
rm rebar.lock
rebar3 esp32_flash -p /dev/ttyUSB0
monitor
