%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  06 Sep 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_ns).

-include("redis_sd.hrl").

%% API
-export([is_label/1, extract/1, join/1, reverse/1, split/1]).

-type dir() :: ltr | rtl.

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
-spec is_label(string()) -> boolean().
is_label([$- | _]) ->
	false;
is_label(Label=[_ | _]) when length(Label) =< 63 ->
	check_label(Label);
is_label(_) ->
	false.

%% @doc Extract the instance from a full service name.
-spec extract(binary() | iolist()) -> {dir(), binary(), binary()}.
extract(List) when is_list(List) ->
	extract(iolist_to_binary(List));
extract(B = << $$, Binary/binary >>) ->
	case binary:split(Binary, << $^ >>, [trim]) of
		[DomainName, Instance] ->
			{rtl, Instance, DomainName};
		_ ->
			erlang:error(badarg, [B])
	end;
extract(Binary) when is_binary(Binary) ->
	case binary:last(Binary) of
		$$ ->
			case binary:split(Binary, << $^ >>, [trim]) of
				[Instance, DomainName] ->
					{ltr, Instance, binary:part(DomainName, 0, byte_size(DomainName) - 1)};
				_ ->
					erlang:error(badarg, [Binary])
			end;
		_ ->
			erlang:error(badarg, [Binary])
	end.

%% @doc Join and urlencode a DNS with dots (.)
-spec join({dir(), iodata(), iodata() | [iodata()]}) -> binary().
join({rtl, Instance, Labels}) ->
	DomainName = ns_join(Labels, [], []),
	Encoded = redis_sd:urlencode(Instance),
	<< $$, DomainName/binary, $^, Encoded/binary >>;
join({ltr, Instance, Labels}) ->
	DomainName = ns_join(Labels, [], []),
	Encoded = redis_sd:urlencode(Instance),
	<< Encoded/binary, $^, DomainName/binary, $$ >>.

%% @doc Reverse the parts of a DNS.
%% For example: <<"instance^_service._type.domain$">> becomes <<"$domain._type._service^instance">>
-spec reverse(binary() | iolist()) -> binary().
reverse(L) when is_list(L) ->
	reverse(iolist_to_binary(L));
reverse(B) when is_binary(B) ->
	reverse(extract(B));
reverse({Direction0, Instance, DomainName}) ->
	Direction = case Direction0 of
		ltr ->
			rtl;
		rtl ->
			ltr
	end,
	Reversed = ns_reverse(DomainName, <<>>, []),
	join({Direction, redis_sd:urldecode(Instance, skip), Reversed}).

%% @doc Split and urldecode a DNS by dots (.)
-spec split(binary() | iolist()) -> {dir(), binary(), [binary()]}.
split(L) when is_list(L) ->
	split(iolist_to_binary(L));
split(B) when is_binary(B) ->
	split(extract(B));
split({Direction, Instance, DomainName}) ->
	{Direction, redis_sd:urldecode(Instance, skip), ns_split(DomainName, <<>>, [])}.

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
		when (C >= $a andalso C =< $z)
		orelse (C >= $0 andalso C =< $9) ->
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
ns_join([[$_ | Name] | Names], Encoded, Tokens) ->
	ns_join(Names, [[$_, redis_sd:urlencode(Name)] | Encoded], Tokens);
ns_join([Name | Names], Encoded, Tokens) ->
	ns_join(Names, [redis_sd:urlencode(Name) | Encoded], Tokens).

%% @private
ns_reverse(L, _Token, []) when is_list(L) ->
	ns_join(lists:reverse(L), [], []);
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
