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
  stop/1
]).

%%==============================================================================
%%	Protocol API
%%==============================================================================
-export([
  create_tag/2, create_tag/3,
  destroy_tag/2, destroy_tag/3,
  read/2, read/3,
  write/2, write/3
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).


-define(HEADER_LENGTH,4).

%%==============================================================================
%%	Control API
%%==============================================================================
start_link(Name) ->
  start_link(Name,#{ timeout => ?CONNECT_TIMEOUT }).
start_link(Name, #{timeout := Timeout } = Options) ->
  Self = self(),
  PID = spawn_link(fun()->init( Name, Self, Options ) end),
  receive
    {PID,connected}-> {ok,PID};
    {'EXIT', PID, Reason}-> {error, Reason}
  after
    Timeout->
      stop(PID),
      { error, timeout }
  end.

stop(PID) ->
  PID ! { self(), stop }.


%%==============================================================================
%%	Protocol API
%%==============================================================================
create_tag(PID, ConnectionStr) ->
  create_tag(PID, ConnectionStr, ?RESPONSE_TIMEOUT).
create_tag(PID, ConnectionStr, Timeout) ->
  transaction(PID, <<"creete">>,ConnectionStr, Timeout).

destroy_tag(PID, TagID) ->
  destroy_tag(PID, TagID, ?RESPONSE_TIMEOUT).
destroy_tag(PID, TagID, Timeout) ->
  transaction(PID, <<"destroy">>, TagID, Timeout).

read(PID, Params) ->
  read(PID, Params, ?RESPONSE_TIMEOUT).
read(PID, Params, Timeout) ->
  transaction(PID, <<"read">>, Params, Timeout).

write(PID, Params) ->
  write(PID, Params, ?RESPONSE_TIMEOUT).
write(PID, Params, Timeout) ->
  transaction(PID, <<"write">>, Params, Timeout).

transaction( PID, Command, Body, Timeout )->
  TID = rand:uniform(16#FFFF),
  Request = #{
    <<"cmd">> => Command,
    <<"tid">> => TID,
    <<"body">> => Body
  },
  PID ! { self(), call, jsx:encode(Request), Timeout },
  wait_for_reply( PID, Command, TID, Timeout ).

wait_for_reply( PID, Command, TID, Timeout )->
  receive
    {PID, reply, {ok, Result} }->
      case try jsx:decode(Result, [return_maps]) catch _:_->{invalid_json, Result } end of
        #{<<"cmd">> := Command, <<"tid">> := TID, <<"reply">> := Reply}->
          case Reply of
            #{<<"type">> := <<"ok">>, <<"result">> := CmdResult}->
              {ok, CmdResult};
            #{<<"type">> := <<"error">>, <<"text">> := Error}->
              {error, Error};
            Unexpected->
              {error, {unexpected_port_reply, Unexpected} }
          end;
        Unexpected->
          ?LOGWARNING("unexpected reply from the port ~p",[Unexpected]),
          wait_for_reply( PID, Command, TID, Timeout )
      end;
    {PID, reply, Error }->
      Error
  after
    Timeout-> {error, timeout}
  end.
%%==============================================================================
%%	Initialization procedure
%%==============================================================================
init( Name, Owner, Options ) ->
  process_flag(trap_exit, true),
  case init_ext_programm( Name ) of
    {ok,Port}->
      Owner ! {self(),connected},
      loop(Port, Owner, Options);
    InitError->
      Owner ! InitError
  end.

init_ext_programm( _Name )->
  try
    Dir = code:priv_dir(?MODULE),
    Source = atom_to_list(?MODULE),
    Program = Dir ++ "/" ++ Source,
    Port = open_port({spawn, Program}, [{packet, ?HEADER_LENGTH}, binary, nouse_stdio]),
    {ok,Port}
  catch
    _:Error->{error,Error}
  end.


%%==============================================================================
%%	THE LOOP
%%==============================================================================
loop( Port, Owner, Options ) ->
  receive
    {From, call, Msg, Timeout} ->
      Result = call( Port, Msg, Options, Timeout ),
      From ! {self(), reply, Result},
      loop(Port,Owner,Options);
    {Port, {data, _Data}}->
      ?LOGWARNING("unexpected data is received from the port"),
      loop(Port, Owner, Options);
    { Owner, stop } ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit({port_terminated, Reason});
    {'EXIT', Owner, Reason} ->
      port_close( Port ),
      exit( Reason );
    Unexpected->
      ?LOGWARNING("Unexpected data is received, data: ~p", [Unexpected]),
      loop(Port, Owner, Options)
  end.

call( Port, Msg, _Options, Timeout )->
  Port ! {self(), {command, Msg}},
  receive
    {Port, {data, Data}} ->
      {ok, Data }
  after
    Timeout->
      {error, timeout}
  end.