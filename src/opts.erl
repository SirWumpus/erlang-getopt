-module(opts).
%-compile(export_all).
-export([to_map/2, get/1, get/2, set/2, dump/0]).

%%
%% Similar to `egetopt:parse/2`, with the side-effect of saving the
%% parsed options as an `ets' table.
%%
%% @see egetopt:parse/2
%%
-spec to_map(egetopt:args(), egetopt:opts()) -> egetopt:retok(egetopt:ret_map()) | egetopt:ret_err().
to_map(Args, Opts) ->
	case egetopt:to_map(Args, Opts) of
	{ok, Options, ArgN} ->
		ets:new(opts_table, [set, named_table]),
		ets:insert(opts_table, maps:to_list(Options)),
		{ok, Options, ArgN};
	Other ->
		Other
	end.

%%
%% Get the `OptName` from the `ets` table returning a value.
%%
-spec get(egetopt:optname()) -> egetopt:optarg().
get(OptName) ->
	get(OptName, undefined).

%%
%% Get the `OptName` from the `ets` table returning a value; otherwise `Default`.
%%
-spec get(egetopt:optname(), egetopt:optarg()) -> egetopt:optarg().
get(OptName, Default) ->
	case ets:lookup(opts_table, OptName) of
	[{OptName, Value}] -> Value;
	[] -> Default
	end.

%%
%% Set the value of an `OptName` in the `ets` table.
%%
-spec set(egetopt:optname(), egetopt:optarg()) -> true.
set(OptName, OptArg) ->
	ets:insert(opts_table, {OptName, OptArg}).

%%
%% Dump the options defined in the `ets` table.
%%
-spec dump() -> egetopt:ret_plist().
dump() ->
	ets:tab2list(opts_table).
