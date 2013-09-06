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

-record(redis_sd, {
	ttl      = undefined :: undefined | non_neg_integer(),
	domain   = undefined :: undefined | binary(),
	type     = undefined :: undefined | binary(),
	service  = undefined :: undefined | binary(),
	hostname = undefined :: undefined | binary(),
	instance = undefined :: undefined | binary(),
	target   = undefined :: undefined | binary(),
	port     = undefined :: undefined | non_neg_integer(),
	txtdata  = undefined :: undefined | [{binary(), binary()}]
}).
