%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  06 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_ns).

-include("redis_sd.hrl").

%% API
-export([is_label/1, join/1, reverse/1, split/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Validates whether Label is a valid hostname label string().
%% Must contain: lowercase a-z, 0-9, and hyphen (-).
%% Must NOT start or end with hyphen (-).
%% Must be at least 1 byte in length.
%% Must NOT be longer than 63 bytes.
%% [http://tools.ietf.org/html/rfc952]
%% [http://tools.ietf.org/html/rfc1123]
-spec is_label(string() | binary()) -> boolean().
is_label(Binary) when is_binary(Binary) ->
	is_label(binary_to_list(Binary));
is_label([$- | _]) ->
	false;
is_label(Label=[_ | _]) when length(Label) =< 63 ->
	check_label(Label);
is_label(_) ->
	false.

%% @doc Join Labels with dots (.)
-spec join(binary() | [iodata()]) -> binary().
join(Labels) ->
	ns_join(Labels, [], []).

%% @doc Reverse the parts of a DNS.
%% For example: <<"_service._type.domain">> becomes <<"domain._type._service">>
-spec reverse(iodata()) -> binary().
reverse(L) when is_list(L) ->
	reverse(iolist_to_binary(L));
reverse(B) when is_binary(B) ->
	ns_reverse(B, <<>>, []).

%% @doc Split a DNS by dots (.)
-spec split(iodata()) -> binary().
split(L) when is_list(L) ->
	split(iolist_to_binary(L));
split(B) when is_binary(B) ->
	ns_split(B, <<>>, []).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
check_label([]) ->
	true;
check_label([$-]) ->
	false;
check_label([$- | Label]) ->
	check_label(Label);
check_label([C | Label])
		when is_integer(C) andalso
		((C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9)) ->
	check_label(Label);
check_label(_) ->
	false.

%% @private
ns_join(Bin, [], _Tokens) when is_binary(Bin) ->
	Bin;
ns_join([], [], Tokens) ->
	iolist_to_binary(Tokens);
ns_join([], [Token | Encoded], []) ->
	ns_join([], Encoded, [Token]);
ns_join([], [Token | Encoded], Tokens) ->
	ns_join([], Encoded, [Token, $. | Tokens]);
ns_join([Name | Names], Encoded, Tokens) ->
	ns_join(Names, [Name | Encoded], Tokens).

%% @private
ns_reverse(<<>>, Token, Names) ->
	iolist_to_binary([Token | Names]);
ns_reverse(<< $., Rest/binary >>, Token, Names) ->
	ns_reverse(Rest, <<>>, [$., Token | Names]);
ns_reverse(<< C, Rest/binary >>, Token, Names) ->
	ns_reverse(Rest, << Token/binary, C >>, Names).

%% @private
ns_split(<<>>, Token, Names) ->
	lists:reverse([Token | Names]);
ns_split(<< $., Rest/binary >>, Token, Names) ->
	ns_split(Rest, <<>>, [Token | Names]);
ns_split(<< C, Rest/binary >>, Token, Names) ->
	ns_split(Rest, << Token/binary, C >>, Names).

%%%-------------------------------------------------------------------
%%% Test functions
%%%-------------------------------------------------------------------

-ifdef(TEST).

is_label_test_() ->
	Specs = [
		{"domain", true},
		{<<"domain">>, true},
		{"0123456789", true},
		{"0", true},
		{"abcdefghijklmnopqrstuvwxyz0123456789", true},
		{"xn--bcher-kva", true},
		{"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", true}, % max 63 chars
		{"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", false},
		{"-hyphen", false},
		{"hyphen-", false},
		{"-hyphen-", false},
		{"dot.domain", false},
		{"doMain", false},
		{"dΩmain", false},
		{domain, false},
		{[$a, bad], false}
	],
	[{lists:flatten(io_lib:format("label: ~p", [Label])), fun() ->
		Result = is_label(Label)
	end} || {Label, Result} <- Specs].

-endif.
