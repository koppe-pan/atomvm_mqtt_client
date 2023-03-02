-module(variables).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([start/1, get_mqtt/0, get_rfid/0, get_ledc/0, set_mqtt/1, set_rfid/1, set_ledc/1]).

-record(state, {
    mqtt,
    rfid,
    ledc
}).

start(Data) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Data, []).

get_mqtt() ->
  gen_server:call(?MODULE, get_mqtt).

get_rfid() ->
  gen_server:call(?MODULE, get_rfid).

get_ledc() ->
  gen_server:call(?MODULE, get_ledc).

set_mqtt(Data) ->
  gen_server:cast(?MODULE, {set_mqtt, Data}).

set_rfid(Data) ->
  gen_server:cast(?MODULE, {set_rfid, Data}).

set_ledc(Data) ->
  gen_server:cast(?MODULE, {set_ledc, Data}).

% gen server
%% @hidden
init({MQTT, RFID, LEDC}) ->
  {ok, #state{mqtt=MQTT, rfid=RFID, ledc=LEDC}}.

%% @hidden
handle_call(get_mqtt, _From, State) ->
  {reply, State#state.mqtt, State};
handle_call(get_rfid, _From, State) ->
  {reply, State#state.rfid, State};
handle_call(get_ledc, _From, State) ->
  {reply, State#state.ledc, State};
handle_call(_Msg, _From, State) ->
  {reply, State, State}.

%% @hidden
handle_cast({set_mqtt, Data}, State) ->
  {noreply, State#state{mqtt=Data}};
handle_cast({set_rfid, Data}, State) ->
  {noreply, State#state{rfid=Data}};
handle_cast({set_ledc, Data}, State) ->
  {noreply, State#state{ledc=Data}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
