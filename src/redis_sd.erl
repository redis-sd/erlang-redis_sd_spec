%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc urldecode/1,2 and urlencode/1,2 are from cowboy_http, require/1 is from ranch
%%% https://github.com/extend/cowboy/blob/0.8.6/src/cowboy_http.erl
%%% https://github.com/extend/ranch/blob/0.8.5/src/ranch.erl
%%%
%%% @end
%%% Created :  30 Aug 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd).

-include("redis_sd.hrl").

%% API
-export([any_to_string/1]).
-export([require/1]). % from ranch
-export([urldecode/1, urldecode/2, urlencode/1, urlencode/2]). % from cowboy_http

%% Object API
-export([new/8, obj_key/1, obj_val/1]).

%% Types
-type obj() :: #redis_sd{}.
-export_type([obj/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Converts anything to a string.
-spec any_to_string(integer() | binary() | atom() | iodata()) -> string().
any_to_string(I) when is_integer(I) ->
	integer_to_list(I);
any_to_string(B) when is_binary(B) ->
	binary_to_list(B);
any_to_string(A) when is_atom(A) ->
	atom_to_list(A);
any_to_string(L) when is_list(L) ->
	any_to_string(iolist_to_binary(L)).

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).

%% @doc Decode a URL encoded binary.
%% @equiv urldecode(Bin, crash)
-spec urldecode(binary() | string()) -> binary().
urldecode(Bin) when is_binary(Bin) ->
	urldecode(Bin, crash).

%% @doc Decode a URL encoded binary.
%% The second argument specifies how to handle percent characters that are not
%% followed by two valid hex characters. Use `skip' to ignore such errors,
%% if `crash' is used the function will fail with the reason `badarg'.
-spec urldecode(binary() | string(), crash | skip) -> binary().
urldecode(List, OnError) when is_list(List) ->
	case catch unicode:characters_to_binary(List) of
		Bin when is_binary(Bin) ->
			urldecode(Bin, OnError);
		_ ->
			urldecode(iolist_to_binary(List), OnError)
	end;
urldecode(Bin, OnError) when is_binary(Bin) ->
	urldecode(Bin, <<>>, OnError).

%% @doc URL encode a string binary.
%% @equiv urlencode(Bin, [])
-spec urlencode(binary() | string()) -> binary().
urlencode(Bin) ->
	urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
-spec urlencode(binary() | string(), [noplus|upper]) -> binary().
urlencode(List, Opts) when is_list(List) ->
	case catch unicode:characters_to_binary(List) of
		Bin when is_binary(Bin) ->
			urlencode(Bin, Opts);
		_ ->
			urlencode(iolist_to_binary(List), Opts)
	end;
urlencode(Bin, Opts) when is_binary(Bin) ->
	Plus = not lists:member(noplus, Opts),
	Upper = lists:member(upper, Opts),
	urlencode(Bin, <<>>, Plus, Upper).

%%%===================================================================
%%% Object API functions
%%%===================================================================

%% @doc Creates a new #redis_sd{} object.
-spec new(integer(), binary(), binary(), binary(), binary(), binary(), integer(), [{binary(), binary()}])
	-> redis_sd:obj().
new(TTL, Domain, Type, Service, Instance, Target, Port, TXTData)
		when is_integer(TTL)
		andalso is_binary(Domain)
		andalso is_binary(Type)
		andalso is_binary(Service)
		andalso is_binary(Instance)
		andalso is_binary(Target)
		andalso is_integer(Port)
		andalso is_list(TXTData) ->
	#redis_sd{
		ttl = TTL,
		domain = Domain,
		type = Type,
		service = Service,
		instance = Instance,
		target = Target,
		port = Port,
		txtdata = TXTData
	}.

%% @doc Return the unqiue key for the #redis_sd{} object.
-spec obj_key(redis_sd:obj())
	-> {binary(), binary(), binary(), binary()}.
obj_key(#redis_sd{domain=D, type=T, service=S, instance=I}) ->
	{D, T, S, I}.

%% @doc Return the value for the #redis_sd{} object.
-spec obj_val(redis_sd:obj())
	-> {integer(), binary(), integer(), [{binary(), binary()}]}.
obj_val(#redis_sd{ttl=TTL, target=Target, port=Port, txtdata=TXTData}) ->
	{TTL, Target, Port, TXTData}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

-spec urldecode(binary(), binary(), crash | skip) -> binary().
%% @private
urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
	G = unhex(H),
	M = unhex(L),
	if  G =:= error; M =:= error ->
		case OnError of skip -> ok; crash -> erlang:error(badarg) end,
		urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
		true ->
		urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
	end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
	case OnError of skip -> ok; crash -> erlang:error(badarg) end,
	urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
	Acc.

-spec unhex(byte()) -> byte() | error.
%% @private
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.

-spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
%% @private
urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
	if  C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
		urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $ , Plus ->
		urlencode(Rest, <<Acc/binary, $+>>, P, U);
		true ->
		H = C band 16#F0 bsr 4, L = C band 16#0F,
		H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
		L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
		urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
	end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
	Acc.

-spec tohexu(byte()) -> byte().
%% @private
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 17 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
%% @private
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.

