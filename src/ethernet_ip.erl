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
  create_tags/3, create_tags/4,
  destroy_tags/2, destroy_tags/3,
  read/3, read/4,
  write/3, write/4,
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
  case eport_c:start_link(SourcePath, Name, Options) of
    {ok, PID} ->
      Ref = ets:new(?MODULE, [set, public]),
      {ok, {PID, Ref}};
    {error, Error} -> {error, Error}
  end.

stop({PID, Ref}) ->
  Tags = ets:match(Ref, '$1'),
  TagIDs = [TagID||{_TagName, TagID} <- Tags],
  destroy_tags(PID, TagIDs),
  ets:delete(Ref),
  eport_c:stop( PID ).

set_log_level(PID, Level)->
  eport_c:set_log_level(PID, Level).

%%==============================================================================
%%	Protocol API
%%==============================================================================
create_tags(Ref, TagNames, Params) ->
  create_tags(Ref, TagNames, Params, ?RESPONSE_TIMEOUT).
create_tags({PID, EtsRef}, TagNames, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC} = Params, Timeout) ->
  if
    length(TagNames) > 0 ->
      ConnectionStr = connection_prefix( Params#{ <<"protocol">> => <<"ab_eip">> }),
      case eport_c:request(PID, <<"create">>,#{<<"tag_names">> => TagNames, <<"tag_string">> => ConnectionStr}, Timeout) of
        {ok, TagIDs}->
          ets:insert(EtsRef, lists:zip( TagNames, TagIDs )),
          {ok, TagIDs};
        Error->
          Error
      end;
    true ->
      {ok, []}
  end;
create_tags(_Ref, _TagNames, InvalidParams, _Timeout) ->
  ?LOGERROR("Params do not contain required param(s) ~p", [InvalidParams]),
  {error, {invalid_params, InvalidParams}}.

destroy_tags(PID, TagIDS) ->
  destroy_tags(PID, TagIDS, ?RESPONSE_TIMEOUT).
destroy_tags(PID, TagIDS, Timeout) ->
  eport_c:request(PID, <<"destroy">>, TagIDS, Timeout).

read(Ref, TagList, Params) ->
  read(Ref, TagList, Params, ?RESPONSE_TIMEOUT).
read({PID, _EtsRef} = Ref, TagList, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC} = Params, Timeout)->
  TagIDs = names2ids(Ref, Params, [TagName||{TagName, _TagType} <- TagList] ),
  Tags =
    [ #{<<"tag_id">> => TagID, <<"type">> => TagType} || {TagID, TagType} <- lists:zip( TagIDs, [TagType||{_TagName, TagType} <- TagList] )],
  case eport_c:request(PID, <<"read">>, Tags, Timeout) of
    {ok, Result} -> {ok, Result};
    Other -> Other
  end;
read(_Id, _TagNameList, InvalidParams, _Timeout) ->
  ?LOGERROR("Params do not contain required params ~p", [InvalidParams]),
  {error, {invalid_params, InvalidParams}}.

write(Ref, TagsList, Params) ->
  write(Ref, TagsList, Params, ?RESPONSE_TIMEOUT).
write({PID, _EtsRef} = Ref, TagsList, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC}=Params, Timeout)->
  TagIDs = names2ids(Ref, Params, [Name || {Name, _Type, _Value} <- TagsList] ),
  TagsInfo = [#{<<"tag_id">> => TagID, <<"type">> => Type, <<"value">> => Value}
    || {TagID, {_TagName, Type, Value}} <- lists:zip(TagIDs, TagsList)],
  case eport_c:request(PID, <<"write">>, TagsInfo, Timeout) of
    {ok, Result} -> {ok, Result};
    Other -> Other
  end;
write(_Id, _TagNameList, InvalidParams, _Timeout) ->
  ?LOGERROR("Params do not contain required params ~p", [InvalidParams]),
  {error, {invalid_params, InvalidParams}}.

browse_tags({PID, Ref}, Params) ->
  browse_tags({PID, Ref}, Params, ?RESPONSE_TIMEOUT).
browse_tags({PID, _Ref}, #{<<"gateway">>:=_IP, <<"path">>:=_Path,<<"plc">>:=_PLC}=Params, Timeout) ->
  ConnectionStr = connection_prefix(Params#{ <<"protocol">> => <<"ab_eip">> }),
  eport_c:request(PID, <<"browse_tags">>, ConnectionStr, Timeout);
browse_tags(_PID, InvalidParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [InvalidParams]),
  {error, {invalid_params, InvalidParams}}.

%%=====================================================================
%% Internal helpers
%%=====================================================================
connection_prefix( Params )->
  Params1 =
    [ unicode:characters_to_list( K ) ++ "=" ++ unicode:characters_to_list( V ) || {K, V} <- maps:to_list( Params ) ],
  unicode:characters_to_binary( string:join( Params1 ++ ["name="], "&" )).

names2ids({PID, EtsRef}, Params, Names)->
  ToCreate = [ N || N <- Names, ets:lookup(EtsRef, N) =:= []],
  create_tags({PID, EtsRef}, ToCreate, Params),
  [ element(2,hd( ets:lookup( EtsRef, N) )) || N <- Names].
