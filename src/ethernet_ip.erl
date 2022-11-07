%%----------------------------------------------------------------
%% Copyright (c) 2021 Faceplate
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%---------------------
-module(ethernet_ip).

-include("ethernet_ip.hrl").

%%==============================================================================
%%	Control API
%%==============================================================================
-export([
  start_link/1, start_link/2,
  set_log_level/2,
  stop/1
]).

%%==============================================================================
%%	Protocol API
%%==============================================================================
-export([
  create_tags/2, create_tags/3,
  destroy_tags/2, destroy_tags/3,
  read/2, read/3,
  write/2, write/3,
  browse_tags/2, browse_tags/3
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).


-define(HEADER_LENGTH,4).

%%==============================================================================
%%	Control API
%%==============================================================================
start_link(Name) ->
  start_link(Name,#{ response_timeout => ?RESPONSE_TIMEOUT }).
start_link(Name, Options) ->
  Dir=code:priv_dir(ethernet_ip),
  Source=
    case os:type() of
      {unix, linux}->atom_to_list( ?MODULE );
      {win32, _}->atom_to_list( ?MODULE ) ++ ".exe"
    end,
  SourcePath = unicode:characters_to_binary(Dir ++ "/" ++ Source),
  eport_c:start_link(SourcePath, Name, Options).

stop(PID) ->
  eport_c:stop( PID ).

set_log_level(PID, Level)->
  eport_c:set_log_level(PID, Level).
%%==============================================================================
%%	Protocol API
%%==============================================================================
create_tags(PID, Params) ->
  create_tags(PID, Params, ?RESPONSE_TIMEOUT).
create_tags(PID, #{<<"tag_names">>:=TagNames, <<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC} = Params, Timeout) ->
  Params1 = Params#{<<"protocol">> => <<"ab_eip">>},
  ConnectionStr =
    maps:fold(
      fun(Key, Value, ConnStr) ->
        if Key =/= <<"tag_names">> ->
            KeyValue = <<Key/binary, "=", Value/binary>>,
            case ConnStr of
              <<>> -> KeyValue;
              ConnStr -> <<ConnStr/binary, "&", KeyValue/binary>>
            end;
          true -> ConnStr
        end
      end, <<>>, Params1),
  ConnectionStrBase = <<ConnectionStr/binary, "&name=">>,
  eport_c:request(PID, <<"create">>,#{<<"tag_names">> => TagNames, <<"tag_string">> => ConnectionStrBase}, Timeout);
create_tags(_PID, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

destroy_tags(PID, TagIDS) ->
  destroy_tags(PID, TagIDS, ?RESPONSE_TIMEOUT).
destroy_tags(PID, TagIDS, Timeout) ->
  eport_c:request(PID, <<"destroy">>, TagIDS, Timeout).

read(PID, Params) ->
  read(PID, Params, ?RESPONSE_TIMEOUT).
read(PID, TagList, Timeout) when is_list(TagList)->
  case eport_c:request(PID, <<"read">>, TagList, Timeout) of
    {ok, Result} ->
      {ok, [base64:decode(R) || R <- Result]};
    Other -> Other
  end;
read(_PID, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

write(PID, Params) ->
  write(PID, Params, ?RESPONSE_TIMEOUT).
write(PID, TagList, Timeout) when is_list(TagList)->
  TagList1 =
    [begin
     EncodedValue = base64:encode(Value),
     Info#{<<"value">> => EncodedValue, <<"length">> => byte_size(EncodedValue)}
    end || #{<<"value">> := Value}=Info <- TagList],
  eport_c:request(PID, <<"write">>, TagList1, Timeout);
write(_PID, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

browse_tags(PID, Params) ->
  browse_tags(PID, Params, ?RESPONSE_TIMEOUT).
browse_tags(PID, #{<<"gateway">>:=_IP, <<"path">>:=_Path,<<"plc">>:=_PLC}=Params, Timeout) ->
  Params1 = Params#{<<"protocol">> => <<"ab_eip">>},
  ConnectionStr =
    maps:fold(
      fun(Key, Value, ConnStr) ->
        KeyValue = <<Key/binary, "=", Value/binary>>,
        case ConnStr of
          <<>> -> KeyValue;
          ConnStr -> <<ConnStr/binary, "&", KeyValue/binary>>
        end
      end, <<>>, Params1),
  ConnectionStrBase = <<ConnectionStr/binary, "&name=">>,
  eport_c:request(PID, <<"browse_tags">>,ConnectionStrBase, Timeout);
browse_tags(_PID, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

%%transaction( PID, Command, Body, Timeout )->
%%  TID = rand:uniform(16#FFFF),
%%  Request = #{
%%    <<"cmd">> => Command,
%%    <<"tid">> => TID,
%%    <<"body">> => Body
%%  },
%%  PID ! { self(), call, jsx:encode(Request), Timeout },
%%  wait_for_reply( PID, Command, TID, Timeout ).

%%wait_for_reply( PID, Command, TID, Timeout )->
%%  receive
%%    {PID, reply, {ok, Result} }->
%%      case try jsx:decode(Result, [return_maps]) catch _:_->{invalid_json, Result } end of
%%        #{<<"cmd">> := Command, <<"tid">> := TID, <<"reply">> := Reply}->
%%          case Reply of
%%            #{<<"type">> := <<"ok">>, <<"result">> := CmdResult}->
%%              {ok, CmdResult};
%%            #{<<"type">> := <<"error">>, <<"text">> := Error}->
%%              {error, Error};
%%            Unexpected->
%%              {error, {unexpected_port_reply, Unexpected} }
%%          end;
%%        Unexpected->
%%          ?LOGWARNING("unexpected reply from the port ~p",[Unexpected]),
%%          wait_for_reply( PID, Command, TID, Timeout )
%%      end;
%%    {PID, reply, Error }->
%%      Error
%%  after
%%    Timeout-> {error, timeout}
%%  end.
%%==============================================================================
%%	Initialization procedure
%%==============================================================================
%%init( Name, Owner, Options ) ->
%%  process_flag(trap_exit, true),
%%  case init_ext_programm( Name ) of
%%    {ok,Port}->
%%      Owner ! {self(),connected},
%%      loop(Port, Owner, Options);
%%    InitError->
%%      Owner ! InitError
%%  end.
%%
%%init_ext_programm( _Name )->
%%  try
%%    Dir = code:priv_dir(?MODULE),
%%    Source = atom_to_list(?MODULE),
%%    Program = Dir ++ "/" ++ Source,
%%    Port = open_port({spawn, Program}, [{packet, ?HEADER_LENGTH}, binary, nouse_stdio]),
%%    {ok,Port}
%%  catch
%%    _:Error->{error,Error}
%%  end.


%%==============================================================================
%%	THE LOOP
%%==============================================================================
%%loop( Port, Owner, Options ) ->
%%  receive
%%    {From, call, Msg, Timeout} ->
%%      Result = call( Port, Msg, Options, Timeout ),
%%      From ! {self(), reply, Result},
%%      loop(Port,Owner,Options);
%%    {Port, {data, _Data}}->
%%      ?LOGWARNING("unexpected data is received from the port"),
%%      loop(Port, Owner, Options);
%%    { Owner, stop } ->
%%      Port ! {self(), close},
%%      receive
%%        {Port, closed} -> exit(normal)
%%      end;
%%    {'EXIT', Port, Reason} ->
%%      exit({port_terminated, Reason});
%%    {'EXIT', Owner, Reason} ->
%%      port_close( Port ),
%%      exit( Reason );
%%    Unexpected->
%%      ?LOGWARNING("Unexpected data is received, data: ~p", [Unexpected]),
%%      loop(Port, Owner, Options)
%%  end.
%%
%%call( Port, Msg, _Options, Timeout )->
%%  Port ! {self(), {command, Msg}},
%%  receive
%%    {Port, {data, Data}} ->
%%      {ok, Data }
%%  after
%%    Timeout->
%%      {error, timeout}
%%  end.