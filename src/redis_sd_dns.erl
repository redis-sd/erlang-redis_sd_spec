%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  28 Jan 2014 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_dns).

-include("redis_sd_dns.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([new/10, get/2, set/2]).
-export([to_binary/1, to_dns_rec/1, from_binary/1, from_dns_rec/1]).

%% Public Types
-type rec() :: redis_sd_dns().
-export_type([rec/0]).

%% Private Types
-type rsd_key() :: domain | type | service | instance | ttl | priority |
	weight | port | target | txtdata.

-type rsd_val() :: rsd_domain() | rsd_type() | rsd_service() |
	rsd_instance() | rsd_ttl() | rsd_priority() | rsd_weight() |
	rsd_port() | rsd_target() | rsd_txtdata().

-type rsd_keyval() ::
	{domain, rsd_domain()} |
	{type, rsd_type()} |
	{service, rsd_service()} |
	{instance, rsd_instance()} |
	{ttl, rsd_ttl()} |
	{priority, rsd_priority()} |
	{weight, rsd_weight()} |
	{port, rsd_port()} |
	{target, rsd_target()} |
	{txtdata, rsd_txtdata()}.

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new(Domain::rsd_domain(), Type::rsd_type(), Service::rsd_service(),
		Instance::rsd_instance(), TTL::rsd_ttl(), Priority::rsd_priority(),
		Weight::rsd_weight(), Port::rsd_port(), Target::rsd_target(),
		TXTData::rsd_txtdata())
	-> redis_sd_dns().
new(Domain, Type, Service, Instance, TTL, Priority, Weight, Port, Target, TXTData) ->
	?REDIS_SD_DNS{domain=Domain, type=Type, service=Service,
		instance=Instance, ttl=TTL, priority=Priority, weight=Weight,
		port=Port, target=Target, txtdata=TXTData}.

-spec get(Key::rsd_key() | [rsd_key()], DNS::redis_sd_dns())
	-> Val::rsd_val() | [{rsd_key(), rsd_val()}].
get(Keys=[Key | _], D=?REDIS_SD_DNS{}) when is_atom(Key) ->
	lists:foldr(fun(K, Acc) ->
		[{K, get(K, D)} | Acc]
	end, [], Keys);
get(domain, ?REDIS_SD_DNS{domain=Domain}) ->
	Domain;
get(type, ?REDIS_SD_DNS{type=Type}) ->
	Type;
get(service, ?REDIS_SD_DNS{service=Service}) ->
	Service;
get(instance, ?REDIS_SD_DNS{instance=Instance}) ->
	Instance;
get(ttl, ?REDIS_SD_DNS{ttl=TTL}) ->
	TTL;
get(priority, ?REDIS_SD_DNS{priority=Priority}) ->
	Priority;
get(weight, ?REDIS_SD_DNS{weight=Weight}) ->
	Weight;
get(port, ?REDIS_SD_DNS{port=Port}) ->
	Port;
get(target, ?REDIS_SD_DNS{target=Target}) ->
	Target;
get(txtdata, ?REDIS_SD_DNS{txtdata=TXTData}) ->
	TXTData.

-spec set(KeyVal::rsd_keyval() | [rsd_keyval()], OldDNS::redis_sd_dns())
	-> NewDNS::redis_sd_dns().
set(KeyVals=[{Key, _Val} | _], D0=?REDIS_SD_DNS{}) when is_atom(Key) ->
	lists:foldl(fun(KV={_, _}, D) ->
		set(KV, D)
	end, D0, KeyVals);
%% Domain
set({domain, Domain}, D=?REDIS_SD_DNS{}) when is_binary(Domain) ->
	D?REDIS_SD_DNS{domain=Domain};
set({type, Type}, D=?REDIS_SD_DNS{}) when is_binary(Type) ->
	D?REDIS_SD_DNS{type=Type};
set({service, Service}, D=?REDIS_SD_DNS{}) when is_binary(Service) ->
	D?REDIS_SD_DNS{service=Service};
set({instance, Instance}, D=?REDIS_SD_DNS{}) when is_binary(Instance) ->
	D?REDIS_SD_DNS{instance=Instance};
set({ttl, TTL}, D=?REDIS_SD_DNS{}) when is_integer(TTL) andalso TTL >= 0 ->
	D?REDIS_SD_DNS{ttl=TTL};

%% SRV
set({priority, Priority}, D=?REDIS_SD_DNS{}) when is_integer(Priority) andalso Priority >= 0 ->
	D?REDIS_SD_DNS{priority=Priority};
set({weight, Weight}, D=?REDIS_SD_DNS{}) when is_integer(Weight) andalso Weight >= 0 ->
	D?REDIS_SD_DNS{weight=Weight};
set({port, Port}, D=?REDIS_SD_DNS{}) when is_integer(Port) andalso Port >= 0 ->
	D?REDIS_SD_DNS{port=Port};
set({target, Target}, D=?REDIS_SD_DNS{}) when is_binary(Target) ->
	D?REDIS_SD_DNS{target=Target};

%% TXT
set({txtdata, TXTData=[]}, D=?REDIS_SD_DNS{}) ->
	D?REDIS_SD_DNS{txtdata=TXTData};
set({txtdata, TXTData=[{Key, Val} | _]}, D=?REDIS_SD_DNS{}) when is_binary(Key) andalso is_binary(Val) ->
	D?REDIS_SD_DNS{txtdata=TXTData}.

-spec to_binary(redis_sd_dns())
	-> binary().
to_binary(D=?REDIS_SD_DNS{}) ->
	inet_dns:encode(to_dns_rec(D)).

-spec to_dns_rec(redis_sd_dns())
	-> term().
to_dns_rec(D=?REDIS_SD_DNS{domain=Domain, type=Type, service=Service, instance=Instance}) ->
	Keys = redis_sd:labels_to_keys({Domain, Type, Service, Instance}),
	PTR = proplists:get_value(ptr, Keys),
	SRV = proplists:get_value(srv, Keys),
	inet_dns:make_msg([
		{header, inet_dns_header()},
		{anlist, inet_dns_anlist(PTR, SRV, D)},
		{arlist, inet_dns_arlist(SRV, D)}
	]).

-spec from_binary(Binary::binary())
	-> redis_sd_dns().
from_binary(Binary) when is_binary(Binary) ->
	{ok, Record} = inet_dns:decode(Binary),
	from_dns_rec(Record).

-spec from_dns_rec(Record::term())
	-> redis_sd_dns().
from_dns_rec(Record) ->
	Header = inet_dns:header(inet_dns:msg(Record, header)),
	Type = inet_dns:record_type(Record),
	Questions = [inet_dns:dns_query(Query) || Query <- inet_dns:msg(Record, qdlist)],
	Answers = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, anlist)],
	Authorities = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, nslist)],
	Resources = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)],
	QR = proplists:get_value(qr, Header),
	Opcode = proplists:get_value(opcode, Header),
	case {Header, Type, Questions, Answers, Authorities, Resources, QR, Opcode} of
		{Header, msg, [], [Answer], [], [Service, Text], true, 'query'} ->
			ptr = proplists:get_value(type, Answer),
			PTR = proplists:get_value(domain, Answer),
			TTL = proplists:get_value(ttl, Answer),
			SRV = proplists:get_value(data, Answer),
			srv = proplists:get_value(type, Service),
			SRV = proplists:get_value(domain, Service),
			TTL = proplists:get_value(ttl, Service),
			{Priority, Weight, Port, Target} = proplists:get_value(data, Service),
			txt = proplists:get_value(type, Text),
			SRV = proplists:get_value(domain, Text),
			TTL = proplists:get_value(ttl, Text),
			TXTData = inet_dns_txtdata_decode(proplists:get_value(data, Text)),
			Keys = [{ptr, redis_sd:any_to_binary(PTR)}, {srv, redis_sd:any_to_binary(SRV)}],
			{RDomain, RType, RService, RInstance} = redis_sd:keys_to_labels(Keys),
			new(RDomain, RType, RService, RInstance, TTL, Priority, Weight, Port, redis_sd:any_to_binary(Target), TXTData);
		_ ->
			erlang:error(badarg, [Record])
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
inet_dns_header() ->
	inet_dns:make_header([
		{id, 0},
		{qr, true},
		{opcode, 'query'},
		{aa, true},
		{tc, false},
		{rd, false},
		{ra, false},
		{pr, false},
		{rcode, 0}
	]).

%% @private
inet_dns_anlist(PTR, SRV, DNS) ->
	[inet_dns_ptr(PTR, SRV, DNS)].

%% @private
inet_dns_ptr(PTR, SRV, ?REDIS_SD_DNS{ttl=TTL}) ->
	inet_dns:make_rr([
		{type, ptr},
		{domain, redis_sd:any_to_string(PTR)},
		{class, in},
		{ttl, TTL},
		{data, redis_sd:any_to_string(SRV)}
	]).

%% @private
inet_dns_arlist(SRV, DNS) ->
	[inet_dns_srv(SRV, DNS), inet_dns_txt(SRV, DNS)].

%% @private
inet_dns_srv(SRV, ?REDIS_SD_DNS{ttl=TTL, priority=Priority, weight=Weight, port=Port, target=Target}) ->
	inet_dns:make_rr([
		{type, srv},
		{domain, redis_sd:any_to_string(SRV)},
		{class, in},
		{ttl, TTL},
		{data, {Priority, Weight, Port, redis_sd:any_to_string(Target)}}
	]).

%% @private
inet_dns_txt(SRV, DNS=?REDIS_SD_DNS{ttl=TTL}) ->
	inet_dns:make_rr([
		{type, txt},
		{domain, redis_sd:any_to_string(SRV)},
		{class, in},
		{ttl, TTL},
		{data, inet_dns_txtdata_encode(DNS)}
	]).

%% @private
inet_dns_txtdata_encode(?REDIS_SD_DNS{txtdata=TXTData}) ->
	[begin
		redis_sd:any_to_string([redis_sd:urlencode(Key), $=, redis_sd:urlencode(Val)])
	end || {Key, Val} <- TXTData].

%% @private
inet_dns_txtdata_decode(TXTData) ->
	[begin
		[Key, Val] = binary:split(redis_sd:any_to_binary(KeyVal), <<"=">>),
		{redis_sd:urldecode(Key), redis_sd:urldecode(Val)}
	end || KeyVal <- TXTData].

-ifdef(TEST).

roundtrip_test_() ->
	Specs = [
		new(<<"local">>, <<"type">>, <<"service">>, <<"instance">>, 120, 0, 0, 0, <<"host.local">>, [{<<"key">>, <<"val">>}])
	],
	[{lists:flatten(io_lib:format("roundtrip: ~p", [Spec])), fun() ->
		?assertEqual(Spec, from_binary(to_binary(Spec)))
	end} || Spec <- Specs].

-endif.
