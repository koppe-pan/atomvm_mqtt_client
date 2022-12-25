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
    %%
    %% Start the network
    %%
    ok = start_network(maps:get(sta, config:get())),
    %%
    %% Start the MQTT client.
    %%
    Config = #{
        url => maps:get(url, config:get()),
        username => maps:get(username, config:get()),
        password => maps:get(password, config:get()),
        connected_handler => fun handle_connected/1
    },
    {ok, _MQTT} = mqtt_client:start(Config),
    io:format("MQTT started.~n"),

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
    SubscribeTopic = list_to_binary("commands/" ++ maps:get(clientid, config:get()) ++ "/set_interval"),
    io:format("Connected to ~p~n", [maps:get(url, Config)]),
    io:format("Subscribing to ~p...~n", [SubscribeTopic]),
    ok = mqtt_client:subscribe(MQTT, SubscribeTopic, #{
        subscribed_handler => fun handle_subscribed/2,
        data_handler => fun handle_data/3
    }).

handle_subscribed(MQTT, SubscribeTopic) ->
    io:format("Subscribed to ~p.~n", [SubscribeTopic]),
    PublishTopic = list_to_binary("register/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, maps:get(clientid, config:get())).

handle_data(MQTT, SubscribeTopic, Data) ->
    io:format("Received data on topic ~p: ~p ~n", [SubscribeTopic, Data]),
    PublishTopic = list_to_binary("reports/" ++ maps:get(clientid, config:get())),
    _ = mqtt_client:publish(MQTT, PublishTopic, Data).
