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

-ifndef(REDIS_SD_HRL).

-record(dns_sd, {
	%% Domain
	domain   = undefined :: undefined | binary(),
	type     = undefined :: undefined | binary(),
	service  = undefined :: undefined | binary(),
	instance = undefined :: undefined | binary(),
	ttl      = undefined :: undefined | non_neg_integer(),

	%% SRV
	priority = undefined :: undefined | non_neg_integer(),
	weight   = undefined :: undefined | non_neg_integer(),
	port     = undefined :: undefined | non_neg_integer(),
	target   = undefined :: undefined | binary(),

	%% TXT
	txtdata  = undefined :: undefined | [{binary(), binary()}]
}).

-define(REDIS_SD_HRL, 1).

-endif.
