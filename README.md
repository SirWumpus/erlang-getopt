egetopt
=======

Parse list of strings according to POSIX command-line option and argument rules.  There is no support for GNU long option names, see [jcomellas/getopt](https://github.com/jcomellas/getopt) instead.

Data Types
----------

* Args = [ string() ]
* ArgsRemaining = [ string() ]
* Glyph = alpha() | digit()
* MapSpec = #{ Glyph => { OptName, OptType } }
* OptName = atom()
* OptSpec = TupleSpec | MapSpec | StringSpec
* OptType = count | flag | list | param
* Map = #{FlagName => true, CountName => integer() ParamName => string(), ListName => [ stringN(), ..., string1() ] }
* Plist = [ {OptName1, Value1}, ..., {OptNameN, ValueN} ]
* StringSpec = string()
* TupleSpec = [ { Glyph, OptType, OptName } ]

Exports
-------

### parse(Args, OptSpec) -> {ok, Map_or_Plist, ArgsRemaining} | {error, Reason, Glyph}

Parse list of strings according to POSIX command-line option and argument rules.  An argument that starts with a leading hyphen (-) followed by a single character, "-f".  An option is either an option-flag, "-f", or option-parameter, "-x param".  Option-flags can appear together in any order as a list, "-hfg"; an option-parameter can appear at the end of list of option-flags, "-hfgx param" or "-hfgxparam".  Options can appear in any order and be repeated until a "--" argument is seen, which indicates the remainder are only arguments.

The `OptSpec` can be one of three formats:

* The `StringSpec`, similar to `getopt(3)`, is a string consisting of individual characters for option-flags and characters followed by a colon `:` to indicate an option-parameter is to follow.  As an extension, characters followed by a hash `#` for option-count and characters followed by semi-colon `;` for option-list.  The `OptName` used in the returned map is the option glyph prefixed by `opt_`.

* The `MapSpec` is a map keyed by option glyph, whos value is a tuple with `{OptName, OptType}`.

* The `TupleSpec` is a tuple list of `{Glyph, OptType, OptName}`.

The returned result is a map keyed by `OptName`.  Each option's value varies according to the `OptType` given: `flag` simple sets the option to `true`; `count` for the number of times an option flag is repeated; `param` sets an option to the last seen parameter string; and `list` set an option to a list of parameter strings in collected in reverse order.


Example
-------

```
-module(example).
-export([main/1]).

main(Args) ->
        case egetopt:parse(Args, "ab#c:d;") of
        {ok, Options, ArgsN} ->
                io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
        {error, Reason, Opt} ->
                io:format("~s -~c~n", [Reason, Opt])
        end.
```

```
$ example -aa -b -bb -cfoo -done -c replace -d two hey you
parsed #{option_a => true,option_b => 3,option_c => "replace",
option_d => ["two","one"]}, remaining ["hey","you"]

$ example2 -aa -b -bb -cfoo -done -- -c replace -d two hey you
parsed #{some_a => true,some_b => 3,some_c => "foo",some_d => ["one"]},
remaining ["-c", "replace", "-d", "two", "hey", "you"]

$ ./example3 -ab -b - boo boo
parsed #{opt_a => true,opt_b => 2}, remaining ["-","boo","boo"]

$ example -a -b -x -y
unknown option -x
usage: example [-a][-b...][-c param][-d item] ...
-a              set flag true
-b              count repeated occurences
-c param        set parameter
-d item         add item to list

$ example -a -c
missing parameter -c
usage: example [-a][-b...][-c param][-d item] ...
-a              set flag true
-b              count repeated occurences
-c param        set parameter
-d item         add item to list
```

`example` uses the older tuple list specification; `example2` uses the map specification, and `example3` uses a `getopt(3)` style option string specification with the extensions for count and list types.


Copyright
---------

Copyright 2017, 2019 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
