# Initial Flash
```
flash {your IMG_DIR}
```

# Pack Elixir
```
elixirc {your EX_FILE}
PackBEAM {your AVM_FILE} {your BEAM_FILE}
```

# Flash avm
```
run {your AVM_FILE}
```

# Flash rebar3
```
rebar3 packbeam -p
rebar3 esp32_flash -p /dev/ttyUSB0
```

# Monitor
```
monitor
```
