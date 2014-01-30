%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2014, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  30 Jan 2014 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redis_sd_config).

-include("redis_sd.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([env/2, app_env/3, merge/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

env(Key, Default) ->
	get_app_config(redis_sd, Key, Default).

app_env(App, Key, Default) when is_atom(App) ->
	get_app_config(App, Key, env(Key, Default));
app_env([App | Apps], Key, Default) ->
	Self = self(),
	case get_app_config(App, Key, Self) of
		Self ->
			app_env(Apps, Key, Default);
		Val ->
			Val
	end;
app_env([], Key, Default) ->
	env(Key, Default).

merge(App, Defaults, Config) ->
	merge_config(Defaults, App, Config).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
get_app_config(App, Key, Default) ->
	case application:get_env(App, config) of
		{ok, Config} when is_list(Config) ->
			case lists:keyfind(Key, 1, Config) of
				false ->
					Default;
				{Key, Val} ->
					Val
			end;
		_ ->
			Default
	end.

%% @private
merge_config([], _App, Config) ->
	Config;
merge_config([F | Defaults], App, Config) when is_function(F, 1) ->
	merge_config(Defaults, App, F(Config));
merge_config([{Key, Val} | Defaults], App, Config) ->
	Config2 = merge_config_val(Key, Val, Config),
	merge_config(Defaults, App, Config2);
merge_config([{Key, Val, app} | Defaults], App, Config) ->
	Config2 = merge_config_val(Key, app_env(App, Key, Val), Config),
	merge_config(Defaults, App, Config2);
merge_config([{Key, Val, appforce} | Defaults], App, Config) ->
	merge_config(Defaults, App, lists:keystore(Key, 1, Config, {Key, app_env(App, Key, Val)}));
merge_config([{Key, Val, force} | Defaults], App, Config) ->
	merge_config(Defaults, App, lists:keystore(Key, 1, Config, {Key, Val})).

%% @private
merge_config_val(Key, Default, Config) ->
	case lists:keyfind(Key, 1, Config) of
		false ->
			[{Key, Default} | Config];
		_ ->
			Config
	end.

-ifdef(TEST).

-endif.
