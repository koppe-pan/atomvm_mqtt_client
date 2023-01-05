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

start() ->

    RFIDConfig = #{
        rfid_reading_filter => undefined,
        rfid_reading_handler => fun handle_rfid_reading/2
    },
    {ok, RFID} = rfid:start(RFIDConfig),
    io:format("RFID started.~n"),

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

    {ok, _} = variables:start({MQTT, RFID}),
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
    _ = mqtt_client:publish(MQTT, PublishTopic, term_to_binary(RFIDReading)).
