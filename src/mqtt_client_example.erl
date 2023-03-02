%%
%% Copyright (c) 2021 dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(mqtt_client_example).

-export([start/0]).

-include("ledc.hrl").

-define(LEDC_LS_TIMER, ?LEDC_TIMER_0).
-define(LEDC_LS_MODE, ?LEDC_HIGH_SPEED_MODE).
-define(LEDC_LS_CH0_GPIO, 14).
-define(LEDC_LS_CH0_CHANNEL, ?LEDC_CHANNEL_0).
-define(LEDC_LS_CH1_GPIO, 26).
-define(LEDC_LS_CH1_CHANNEL, ?LEDC_CHANNEL_1).
-define(LEDC_LS_CH2_GPIO, 25).
-define(LEDC_LS_CH2_CHANNEL, ?LEDC_CHANNEL_2).
-define(LEDC_LS_CH3_GPIO, 33).
-define(LEDC_LS_CH3_CHANNEL, ?LEDC_CHANNEL_3).
-define(LEDC_DUTY, 3000).
-define(LEDC_FADE_TIME, 100).
-define(LEDC_IDLE_TIME, 80).
-define(SAMPLING_RATE, 22050).



start() ->

    RFIDConfig = #{
        rfid_reading_filter => undefined,
        rfid_reading_handler => fun handle_rfid_reading/2
    },
    {ok, RFID} = rfid:start(RFIDConfig),
    io:format("RFID started.~n"),


    LEDCLSTimer = [
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 1000},
        {speed_mode, ?LEDC_LS_MODE},
        {timer_num, ?LEDC_LS_TIMER}
    ],
    ok = ledc:timer_config(LEDCLSTimer),
    ChannelConfig = #{ buzzer => [{channel, ?LEDC_LS_CH0_CHANNEL},
                                  {duty, 0},
                                  {gpio_num, ?LEDC_LS_CH0_GPIO},
                                  {speed_mode, ?LEDC_LS_MODE},
                                  {hpoint, 0},
                                  {timer_sel, ?LEDC_LS_TIMER}
                                 ],
                       red => [{channel, ?LEDC_LS_CH1_CHANNEL},
                                  {duty, 0},
                                  {gpio_num, ?LEDC_LS_CH1_GPIO},
                                  {speed_mode, ?LEDC_LS_MODE},
                                  {hpoint, 0},
                                  {timer_sel, ?LEDC_LS_TIMER}
                                 ],
                       blue => [{channel, ?LEDC_LS_CH2_CHANNEL},
                                  {duty, 0},
                                  {gpio_num, ?LEDC_LS_CH2_GPIO},
                                  {speed_mode, ?LEDC_LS_MODE},
                                  {hpoint, 0},
                                  {timer_sel, ?LEDC_LS_TIMER}
                                 ],
                       green => [{channel, ?LEDC_LS_CH3_CHANNEL},
                                  {duty, 0},
                                  {gpio_num, ?LEDC_LS_CH3_GPIO},
                                  {speed_mode, ?LEDC_LS_MODE},
                                  {hpoint, 0},
                                  {timer_sel, ?LEDC_LS_TIMER}
                                 ]
                     },
    lists:foreach(fun(Value) -> ledc:channel_config(Value) end, maps:values(ChannelConfig)),
    ok = ledc:fade_func_install(0),
    io:format("LEDC started.~n"),

    %%
    %% Start the network
    %%
    ok = start_network(maps:get(sta, config:get())),
    %%
    %% Start the MQTT client.
    %%
    MQTTConfig = #{
        url => maps:get(url, config:get()),
        username => maps:get(username, config:get()),
        password => maps:get(password, config:get()),
        connected_handler => fun handle_connected/1
    },
    {ok, MQTT} = mqtt_client:start(MQTTConfig),
    io:format("MQTT started.~n"),

    {ok, _} = variables:start({MQTT, RFID, ChannelConfig}),
    io:format("Variables started.~n"),

    loop_forever().

start_network(StaConfig) ->
    case network_fsm:wait_for_sta(StaConfig) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~s Netmask: ~s Gateway: ~s~n",
                [Address, Netmask, Gateway]
            ),
            ok;
        Error ->
            throw({unable_to_start_network, Error})
    end.

loop_forever() ->
    receive
        halt -> halt
    end.

%%
%% connected callback.  This function will be called
%%
handle_connected(MQTT) ->
    Config = mqtt_client:get_config(MQTT),
    io:format("Connected to ~p~n", [maps:get(url, Config)]),
    PublishTopic = list_to_binary("register/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, maps:get(clientid, config:get())),

    SubscribeTopicForEnableWrite = list_to_binary("commands/" ++ maps:get(clientid, config:get()) ++ "/enable_write"),
    io:format("Subscribing to ~p...~n", [SubscribeTopicForEnableWrite]),
    ok = mqtt_client:subscribe(MQTT, SubscribeTopicForEnableWrite, #{
        subscribed_handler => fun handle_subscribed/2,
        data_handler => fun handle_data_for_enable_write/3
    }),
    SubscribeTopicForDisableWrite = list_to_binary("commands/" ++ maps:get(clientid, config:get()) ++ "/disable_write"),
    io:format("Subscribing to ~p...~n", [SubscribeTopicForDisableWrite]),
    ok = mqtt_client:subscribe(MQTT, SubscribeTopicForDisableWrite, #{
        subscribed_handler => fun handle_subscribed/2,
        data_handler => fun handle_data_for_disable_write/3
    }).

handle_subscribed(_MQTT, SubscribeTopic) ->
    io:format("Subscribed to ~p.~n", [SubscribeTopic]).

handle_data_for_enable_write(MQTT, SubscribeTopic, Data) ->
    io:format("Received data on topic ~p: ~p ~n", [SubscribeTopic, Data]),
    RFID = variables:get_rfid(),
    rfid:enable_write_mode(RFID, binary_to_integer(Data)),
    RFIDReading = rfid:latest_reading(RFID),
    io:format("Latest reading ~p ~n", [RFIDReading]),
    PublishTopic = list_to_binary("reports/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, term_to_binary(RFIDReading)).

handle_data_for_disable_write(MQTT, SubscribeTopic, Data) ->
    io:format("Received data on topic ~p: ~p ~n", [SubscribeTopic, Data]),
    RFID = variables:get_rfid(),
    rfid:disable_write_mode(RFID),
    RFIDReading = rfid:latest_reading(RFID),
    io:format("Latest reading ~p ~n", [RFIDReading]),
    PublishTopic = list_to_binary("reports/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, term_to_binary(RFIDReading)).

handle_rfid_reading(_RFID, RFIDReading) ->
    MQTT = variables:get_mqtt(),
    PublishTopic = list_to_binary("reports/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, term_to_binary(RFIDReading)),
    LEDCChannel = variables:get_ledc(),
    Buzzer = maps:get(buzzer, LEDCChannel),
    SpeedMode = proplists:get_value(speed_mode, Buzzer),
    Channel = proplists:get_value(channel, Buzzer),
    ok = ledc:set_fade_with_time(SpeedMode, Channel, ?LEDC_DUTY, ?LEDC_FADE_TIME),
    ok = ledc:fade_start(SpeedMode, Channel, ?LEDC_FADE_NO_WAIT),
    timer:sleep(?LEDC_FADE_TIME),
    ok = ledc:stop(SpeedMode, Channel, 0),
    LED = case maps:get(clientid, config:get()) of
      "red" -> maps:get(red, LEDCChannel);
      "green" -> maps:get(green, LEDCChannel);
      "yellow" -> maps:get(blue, LEDCChannel)
    end,
    LEDSpeedMode = proplists:get_value(speed_mode, LED),
    LEDChannel = proplists:get_value(channel, LED),
    ok = ledc:set_fade_with_time(LEDSpeedMode, LEDChannel, ?LEDC_DUTY, ?LEDC_FADE_TIME),
    ok = ledc:fade_start(LEDSpeedMode, LEDChannel, ?LEDC_FADE_NO_WAIT),
    timer:sleep(?LEDC_IDLE_TIME),
    ok = ledc:stop(LEDSpeedMode, LEDChannel, 0),
    ok = ledc:set_fade_with_time(SpeedMode, Channel, ?LEDC_DUTY, ?LEDC_FADE_TIME),
    ok = ledc:fade_start(SpeedMode, Channel, ?LEDC_FADE_NO_WAIT),
    timer:sleep(?LEDC_FADE_TIME),
    ok = ledc:stop(SpeedMode, Channel, 0).
%    {ok, S} = file:open("../star.raw", [read, binary, raw]),
%    ok = ledc:set_fade_with_time(SpeedMode, Channel, ?LEDC_DUTY, 3000),
%    ok = ledc:fade_start(SpeedMode, Channel, ?LEDC_FADE_NO_WAIT),
%    play_music(S, SpeedMode, Channel, 0),
%    timer:sleep(3000),
%    file:close(S),
%    ok = ledc:stop(SpeedMode, Channel, 0).
%
%play_music(S, SpeedMode, Channel, N) ->
%    file:pread(S, N, N+1),
