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

-define(TAG_STORAGE,'@ethernet_ip_tag_storage@').

%% How many bits takes each data type
-define(type2bit, #{
  <<"BOOL">> => 1,
  <<"SINT">> => 8, <<"USINT">> => 8,
  <<"INT">> => 16, <<"UINT">> => 16,
  <<"DINT">> => 32, <<"UDINT">> => 32,
  <<"LINT">> => 64, <<"ULINT">> => 64,
  <<"REAL">> => 32, <<"LREAL">> => 64
}).
-define(GET_BIT_SIZE(Type), maps:get(Type, ?type2bit)).
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
      Ref = ets:new(?TAG_STORAGE, [set, public]),
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
create_tags(PID, TagNames, Params) ->
  create_tags(PID, TagNames, Params, ?RESPONSE_TIMEOUT).
create_tags(PID, TagNames, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC} = Params, Timeout) ->
  if
    length(TagNames) > 0 ->
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
      eport_c:request(PID, <<"create">>,#{<<"tag_names">> => TagNames, <<"tag_string">> => ConnectionStrBase}, Timeout);
    true ->
      {ok, []}
  end;
create_tags(_PID, _TagNames, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

destroy_tags(PID, TagIDS) ->
  destroy_tags(PID, TagIDS, ?RESPONSE_TIMEOUT).
destroy_tags(PID, TagIDS, Timeout) ->
  eport_c:request(PID, <<"destroy">>, TagIDS, Timeout).

read({PID, Ref}, TagList, Params) ->
  read({PID, Ref}, TagList, Params, ?RESPONSE_TIMEOUT).
read({PID, Ref}, TagList, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC}=Params, Timeout)->
  TagTypes = [TagType||{_TagName, TagType} <- TagList],
  TagNameList = [TagName||{TagName, _TagType} <- TagList],
  {ok, TagIDS} = get_ids({PID, Ref}, TagNameList, Params),
  TagsInfo = [#{<<"tag_id">> => TagID, <<"type">> => TagType} || {TagID, TagType} <- lists:zip(TagIDS, TagTypes)],
  case eport_c:request(PID, <<"read">>, TagsInfo, Timeout) of
    {ok, Result} -> {ok, Result};
    Other -> Other
  end;
read(_Id, _TagNameList, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

write({PID, Ref}, TagsList, Params) ->
  write({PID, Ref}, TagsList, Params, ?RESPONSE_TIMEOUT).
write({PID, Ref}, TagsList, #{<<"gateway">>:=_IP, <<"path">>:=_Path, <<"plc">>:=_PLC}=Params, Timeout)->
  TagNameList = [TagName || {TagName, _Type, _Value} <- TagsList],
  {ok, TagIDS} = get_ids({PID, Ref}, TagNameList, Params),
  TagsInfo = [#{<<"tag_id">> => TagID, <<"type">> => Type, <<"value">> => Value}
    || {TagID, {_TagName, Type, Value}} <- lists:zip(TagIDS, TagsList)],

  case eport_c:request(PID, <<"write">>, TagsInfo, Timeout) of
    {ok, Result} -> {ok, Result};
    Other -> Other
  end;
write(_Id, _TagNameList, WrongParams, _Timeout) ->
  ?LOGERROR("Params do not contain requiered parametr(s) ~p", [WrongParams]),
  {error, {wrong_params, WrongParams}}.

browse_tags({PID, Ref}, Params) ->
  browse_tags({PID, Ref}, Params, ?RESPONSE_TIMEOUT).
browse_tags({PID, _Ref}, #{<<"gateway">>:=_IP, <<"path">>:=_Path,<<"plc">>:=_PLC}=Params, Timeout) ->
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

%%=====================================================================
%% Internal helpers
%%=====================================================================
get_ids({PID, TagStorageRef}, TagNameLists, Params) ->
  Created_NotCreatedTagsInfo =
    lists:foldl(
      fun(TagName, #{<<"created">> := Created, <<"not_created">> := NotCreated}=Acc) ->
        case ets:lookup(TagStorageRef, TagName) of
          [{TagName, TagID}] ->
            Created1 = Created#{TagName => TagID},
            Acc#{<<"created">> => Created1};
          [] ->
            NotCreated1 = [TagName | NotCreated],
            Acc#{<<"not_created">> => NotCreated1}
        end
      end, #{<<"created">> => #{}, <<"not_created">> => []}, TagNameLists),

  NotCreatedTags = maps:get(<<"not_created">>, Created_NotCreatedTagsInfo),
  {ok, TagIDs} = create_tags(PID, NotCreatedTags, Params),

  NowCreatedTagsInfo = lists:zip(NotCreatedTags, TagIDs),

  ets:insert(TagStorageRef, NowCreatedTagsInfo),

  NowCreatedTagsMap = maps:from_list(NowCreatedTagsInfo),

  BeforeCreatedTagsMap = maps:get(<<"created">>, Created_NotCreatedTagsInfo),
  AllTags = maps:merge(BeforeCreatedTagsMap, NowCreatedTagsMap),
  {ok, [maps:get(TagName, AllTags) || TagName <- TagNameLists]}.

%%parse_type(<<"BOOL">>, BitPos, Value) ->
%%  <<V:8>> = Value, (V bsr BitPos) band 1;
%%parse_type(Type, Size, Value) when Type == <<"SINT">>; Type == <<"INT">>; Type == <<"DINT">>; Type == <<"LINT">>->
%%  <<V:Size/little-signed-integer>> = Value,
%%  V;
%%parse_type(Type, Size, Value) when Type == <<"USINT">>; Type == <<"UINT">>; Type == <<"UDINT">>; Type == <<"ULINT">> ->
%%  <<V:Size/little-unsigned-integer>> = Value,
%%  V;
%%parse_type(Type, Size, Value) when Type == <<"REAL">>; Type == <<"LREAL">>->
%%  <<V:Size/little-float>> = Value,
%%  V;
%%parse_type(_Type, Size, Value) ->
%%  <<V:Size>> = Value,
%%  V.
%%
%%
%%pipe(Pipe,Acc)->
%%  pipe(Pipe,Acc,1).
%%pipe([H|T],Acc,Step)->
%%  case try H(Acc) catch _:E:Stack->{error,{programming_error,E,Stack}} end of
%%    {ok,Acc1}->pipe(T,Acc1,Step+1);
%%    ok->pipe(T,Acc,Step+1);
%%    error->{error,Step,undefined};
%%    {error,Error}->{error,Step,Error};
%%    Acc1->pipe(T,Acc1,Step+1)
%%  end;
%%pipe([],Acc,_Step)->
%%  {ok,Acc}.
%%
%%
%%%---------------Coerce the value to the number---------------------------------
%%value2number(Value) when is_number(Value)->
%%  {ok,Value};
%%value2number(<<"true">>)->
%%  {ok,1};
%%value2number(<<"false">>)->
%%  {ok,0};
%%value2number(true)->
%%  {ok,1};
%%value2number(false)->
%%  {ok,0};
%%value2number(Value) when is_binary(Value)->
%%  try
%%    Int=binary_to_integer(Value),
%%    {ok,Int}
%%  catch
%%    _:_->
%%      try
%%        Float=binary_to_float(Value),
%%        {ok,Float}
%%      catch
%%        _:_->
%%          Size=size(Value),
%%          <<Value1:Size/binary>> = Value,
%%          {ok,Value1}
%%      end
%%  end.
%%
%%%-------------Coerce the number to binary-------------------------------
%%
%%value2bin(<<"BOOL">>, Value)->
%%  {ok,<<(round(Value)):8>>};
%%value2bin(Type, Value) when  Type == <<"USINT">>; Type == <<"UINT">>; Type == <<"UDINT">>; Type == <<"ULINT">> ->
%%  Size = ?GET_BIT_SIZE(Type),
%%  {ok,<<(round(Value)):Size/little-unsigned-integer>>};
%%value2bin(Type, Value) when  Type == <<"SINT">>; Type == <<"INT">>; Type == <<"DINT">>; Type == <<"LINT">> ->
%%  Size = ?GET_BIT_SIZE(Type),
%%  {ok,<<(round(Value)):Size/little-signed-integer>>};
%%value2bin(Type, Value) when  Type == <<"REAL">>; Type == <<"LREAL">> ->
%%  Size = ?GET_BIT_SIZE(Type),
%%  {ok,<<(0.0 + Value):Size/little-float>>}.