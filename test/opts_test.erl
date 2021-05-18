-module(opts_test).
-include_lib("eunit/include/eunit.hrl").

-define(OPTSTRING, "ab#c:d;").

to_map_test_() -> [
	% Errors
	?_assertMatch({error, "unknown option", $x}, opts:to_map(["-x"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, opts:to_map(["-ax"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, opts:to_map(["-a","-x"], ?OPTSTRING)),

	?_assertMatch({error, "missing parameter", $c}, opts:to_map(["-c"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, opts:to_map(["-ac"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, opts:to_map(["-a","-c"], ?OPTSTRING)),

	?_assertMatch({ok,#{opt_b:=1, opt_c:="--"},["baka"]}, opts:to_map(["-c","--","-b","baka"], ?OPTSTRING))
].

parse_and_get1(Args, Opts, OptName) ->
	% Clear previous test.
	ets:delete(opts_table),
	% Create new table.
	{ok, _Map, _ArgsN} = opts:to_map(Args, Opts),
	% Test access.
	opts:get(OptName).

get1_test_() -> [
	?_assertMatch(undefined, parse_and_get1(["-a"], ?OPTSTRING, opt_b)),
	?_assertMatch(3, parse_and_get1(["-bbb"], ?OPTSTRING, opt_b))
].

parse_and_get2(Args, Opts, OptName, Default) ->
	% Clear previous test.
	ets:delete(opts_table),
	% Create new table.
	{ok, _Map, _ArgsN} = opts:to_map(Args, Opts),
	% Test access.
	opts:get(OptName, Default).

get2_test_() -> [
	?_assertMatch(woot, parse_and_get2(["-a"], ?OPTSTRING, opt_b, woot)),
	?_assertMatch(2, parse_and_get2(["-bb"], ?OPTSTRING, opt_b, woot))
].

parse_and_dump(Args, Opts) ->
	% Clear previous test.
	ets:delete(opts_table),
	% Create new table.
	{ok, _Map, _ArgsN} = opts:to_map(Args, Opts),
	% Test access.
	opts:dump().

dump_test_() ->[
	?_assertMatch([{opt_c, "wine"},{opt_a, true}], parse_and_dump(["-a", "-c", "wine"], ?OPTSTRING))
].
