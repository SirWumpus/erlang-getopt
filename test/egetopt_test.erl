-module(egetopt_test).
-include_lib("eunit/include/eunit.hrl").

-define(OPTMAP, #{
	$a => { opt_flag, flag },
	$b => { opt_count, count },
	$c => { opt_param, param },
	$d => { opt_list, list }
}).

-define(OPTSTRING, "ab#c:d;").

parse_test_() -> [
	% Errors
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-x"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-ax"], ?OPTSTRING)),
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-a","-x"], ?OPTSTRING)),

	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-c"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-ac"], ?OPTSTRING)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-a","-c"], ?OPTSTRING)),

	% Defaults
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=nil, opt_d:=[]},["foo","bar"]}, egetopt:parse(["foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=nil, opt_d:=[]},["foo","bar"]}, egetopt:parse(["--","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=false, opt_b:=0, opt_c:=nil, opt_d:=[]},["-", "foo","bar"]}, egetopt:parse(["-","foo","bar"], ?OPTSTRING)),

	% Flag
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:parse(["-a"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:parse(["-aa"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},[]}, egetopt:parse(["-a","-a"], ?OPTSTRING)),

	% Count
	?_assertMatch({ok,#{opt_b:=1},[]}, egetopt:parse(["-b"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=2},[]}, egetopt:parse(["-bb"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true, opt_b:=3},[]}, egetopt:parse(["-bab","-b"], ?OPTSTRING)),

	% Parameter
	?_assertMatch({ok,#{opt_c:="hello"},[]}, egetopt:parse(["-chello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_c:="hello"},[]}, egetopt:parse(["-c","hello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_c:="world"},[]}, egetopt:parse(["-chello","-c","world"], ?OPTSTRING)),

	% List
	?_assertMatch({ok,#{opt_d:=["hello"]},[]}, egetopt:parse(["-dhello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_d:=["hello"]},[]}, egetopt:parse(["-d","hello"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_d:=["world","hello"]},[]}, egetopt:parse(["-dhello","-d","world"], ?OPTSTRING)),

	% Remaining arguments, end arguments "--", and stand alone "-" argument.
	?_assertMatch({ok,#{opt_a:=true},["foo","bar"]}, egetopt:parse(["-a","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},["-b"]}, egetopt:parse(["-a","--","-b"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_a:=true},["-", "-b"]}, egetopt:parse(["-a","-","-b"], ?OPTSTRING)),

	?_assertMatch({ok,#{opt_b:=0, opt_c:="foo"},["bar"]}, egetopt:parse(["-c","foo","bar"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=1, opt_c:="--"},["baka"]}, egetopt:parse(["-c","--","-b","baka"], ?OPTSTRING)),
	?_assertMatch({ok,#{opt_b:=1, opt_c:="-"},["baka"]}, egetopt:parse(["-c","-","-b","baka"], ?OPTSTRING)),

	% All combined.
	?_assertMatch(
		{ok,#{opt_flag:=true,opt_count:=3,opt_param:="replace",opt_list:=["two","one"]},["hey","you"]},
		egetopt:parse(["-aa","-b","-bb","-cfoo","-done","-c","replace","-d","two","hey","you"], ?OPTMAP)
	)
].
