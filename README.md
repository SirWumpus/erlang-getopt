egetopt
=======

Parse list of strings according to POSIX command-line option and argument rules.  There is no support for GNU long option names, see [jcomellas/getopt](https://github.com/jcomellas/getopt) instead.

Data Types
----------

* Args = [ string() ]
* Glyph = alpha() | digit()
* Key = atom()
* OptSpec = [ { Glyph, OptType, Key } ]
* OptType = count | flag | list | param
* Plist = [ { Key, Value } ]
* Value = string()

Exports
-------

### parse(Args1, OptSpec) -> {ok, Plist, Args2} | {error, Reason, Glyph}

Parse list of strings according to POSIX command-line option and argument rules.  An argument that starts with a leading hyphen (-) followed by a single character, "-f".  An option is either an option-flag, "-f", or option-parameter, "-x param".  Option-flags can appear together in any order as a list, "-hfg"; an option-argument can appear at the end of list of option-flags, "-hfgx arg" or "-hfgxparam".  Options can appear in any order until a "--" argument is seen, which indicates the remainder are only arguments.

`OptType` allows for certain common option idioms:

* `count` repeated option-flag, "-f -f -f" or "-fgf".
* `flag` set option-flag, "-f", true.
* `list` to push repeated options onto a list, "-Idir1 -Idir2".
* `param` for an option-parameter, "-x param"; repeats replace previous param.


Example
-------

```
-module(example).
-export([main/1]).

main(Args) ->
	case egetopt:parse(Args, [
		{ $a, flag, opt_a },
		{ $b, count, opt_b },
		{ $c, param, opt_c },
		{ $d, list, opt_d }
	]) of
	{ok, Options, ArgsN} ->
		io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt])
	end.
```

```
$ example -aa -b -bb -cfoo -done -c replace -d two hey you
parsed [{opt_d,["two","one"]},{opt_c,"replace"},{opt_b,3},{opt_a,true}], remaining ["hey","you"]

$ example -aa -b -bb -cfoo -done -- -c replace -d two hey you
parsed [{opt_d,["one"]},{opt_c,"foo"},{opt_b,3},{opt_a,true}], remaining ["-c","replace","-d",
"two","hey","you"]

$ example -ab -b - boo boo
parsed [{opt_b,2},{opt_a,true}], remaining ["-","boo","boo"]

$ example -a -b -x -y
unknown option -x

$ example -a -c
missing argument -c
```


Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.

MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
