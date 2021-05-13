-module(egetopt_test).
-include_lib("eunit/include/eunit.hrl").

-define(OPTMAP, #{
	$a => { opt_flag, flag },
	$b => { opt_count, count },
	$c => { opt_param, param },
	$d => { opt_list, list }
}).

-define(OPTSTRING, "ab#c:d;").

-define(OPTTUPLE, [
	{ $a, flag,  opt_flag },
	{ $b, count, opt_count },
	{ $c, param, opt_param },
	{ $d, list,  opt_list }
]).

to_map_test_() -> [
	% Errors
	?_assertMatch({error, "unknown option", $x}, egetopt:to_map(["-x"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, egetopt:to_map(["-ax"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, egetopt:to_map(["-a","-x"], ?OPTSTRING)),

	?_assertMatch({error, "missing parameter", $c}, egetopt:to_map(["-c"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:to_map(["-ac"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:to_map(["-a","-c"], ?OPTSTRING)),

	% Defaults
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=undefined, opt_d:=[]},["foo","bar"]}, egetopt:to_map(["foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=undefined, opt_d:=[]},["foo","bar"]}, egetopt:to_map(["--","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=undefined, opt_d:=[]},["-", "foo","bar"]}, egetopt:to_map(["-","foo","bar"], ?OPTSTRING)),

	% Flag
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:to_map(["-a"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:to_map(["-aa"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:to_map(["-a","-a"], ?OPTSTRING)),

	% Count
	?_assertMatch({ok,#{opt_b:=1},[]}, egetopt:to_map(["-b"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=2},[]}, egetopt:to_map(["-bb"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true, opt_b:=3},[]}, egetopt:to_map(["-bab","-b"], ?OPTSTRING)),

	% Parameter
	?_assertMatch({ok,#{opt_c:="hello"},[]}, egetopt:to_map(["-chello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_c:="hello"},[]}, egetopt:to_map(["-c","hello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_c:="world"},[]}, egetopt:to_map(["-chello","-c","world"], ?OPTSTRING)),

	% List
	?_assertMatch({ok,#{opt_d:=["hello"]},[]}, egetopt:to_map(["-dhello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_d:=["hello"]},[]}, egetopt:to_map(["-d","hello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_d:=["world","hello"]},[]}, egetopt:to_map(["-dhello","-d","world"], ?OPTSTRING)),

	% Remaining arguments, end arguments "--", and stand alone "-" argument.
	?_assertMatch({ok,#{opt_a:=true},["foo","bar"]}, egetopt:to_map(["-a","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},["-b"]}, egetopt:to_map(["-a","--","-b"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},["-", "-b"]}, egetopt:to_map(["-a","-","-b"], ?OPTSTRING)),

	?_assertMatch({ok,#{opt_b:=0, opt_c:="foo"},["bar"]}, egetopt:to_map(["-c","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=1, opt_c:="--"},["baka"]}, egetopt:to_map(["-c","--","-b","baka"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=1, opt_c:="-"},["baka"]}, egetopt:to_map(["-c","-","-b","baka"], ?OPTSTRING)),

	% All combined.
	?_assertMatch(
		{ok,#{opt_flag:=true,opt_count:=3,opt_param:="replace",opt_list:=["two","one"]},["hey","you"]},
		egetopt:to_map(["-aa","-b","-bb","-cfoo","-done","-c","replace","-d","two","hey","you"], ?OPTMAP)
	),

	?_assertMatch(
		{ok,#{opt_flag:=true,opt_count:=3,opt_param:="replace",opt_list:=["two","one"]},["hey","you"]},
		egetopt:to_map(["-aa","-b","-bb","-cfoo","-done","-c","replace","-d","two","hey","you"], ?OPTTUPLE)
	)
].

parse_test_() -> [
	?_assertMatch(
		{error, "unknown option", $x},
		egetopt:parse(["-x"], ?OPTSTRING)
	),
	?_assertMatch(
		{ok,[{opt_a,false},{opt_b,0},{opt_c,"world"},{opt_d,[]}],[]},
		egetopt:parse(["-chello","-c","world"], ?OPTSTRING)
	),
	?_assertMatch(
		{ok,[{opt_a,false},{opt_b,0},{opt_c,undefined},{opt_d,["world","hello"]}],[]},
		egetopt:parse(["-dhello","-d","world"], ?OPTSTRING)
	),
	?_assertMatch(
		{ok,[{opt_count,3},{opt_flag,true},{opt_list,["two","one"]},{opt_param,"replace"}],["hey","you"]},
		egetopt:parse(["-aa","-b","-bb","-cfoo","-done","-c","replace","-d","two","hey","you"], ?OPTMAP)
	)
].