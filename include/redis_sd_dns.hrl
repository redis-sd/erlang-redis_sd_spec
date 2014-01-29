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

-ifndef(REDIS_SD_DNS_HRL).

%% Domain
-type rsd_domain()   :: iodata().
-type rsd_type()     :: iodata().
-type rsd_service()  :: iodata().
-type rsd_instance() :: iodata().
-type rsd_ttl()      :: non_neg_integer().

%% SRV
-type rsd_priority() :: non_neg_integer().
-type rsd_weight()   :: non_neg_integer().
-type rsd_port()     :: non_neg_integer().
-type rsd_target()   :: iodata().

%% TXT
-type rsd_txtdata()  :: [{iodata(), iodata()}].

-record(redis_sd_dns_v1, {
	%% Domain
	domain   = undefined :: undefined | rsd_domain(),
	type     = undefined :: undefined | rsd_type(),
	service  = undefined :: undefined | rsd_service(),
	instance = undefined :: undefined | rsd_instance(),
	ttl      = undefined :: undefined | rsd_ttl(),

	%% SRV
	priority = undefined :: undefined | rsd_priority(),
	weight   = undefined :: undefined | rsd_weight(),
	port     = undefined :: undefined | rsd_port(),
	target   = undefined :: undefined | rsd_target(),

	%% TXT
	txtdata  = undefined :: undefined | rsd_txtdata()
}).

-type redis_sd_dns() :: #redis_sd_dns_v1{}.

-define(REDIS_SD_DNS, #redis_sd_dns_v1).

-define(REDIS_SD_DNS_HRL, 1).

-endif.
