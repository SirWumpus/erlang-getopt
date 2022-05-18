egetopt
=======

Parse a list of strings according to POSIX command-line option and argument rules.  There is no support for GNU long option names, see [jcomellas/getopt](https://github.com/jcomellas/getopt) instead.


Data Types
----------

* Args = [ string() ]
* ArgsRemaining = [ string() ]
* Glyph = alpha() | digit()
* MapSpec = #{ Glyph => { OptName, OptType } }
* OptName = atom()
* OptSpec = MapSpec | StringSpec | TupleSpec
* OptType = count | flag | list | param
* Map = #{FlagName => true, CountName => integer() ParamName => string(), ListName => [ stringN(), ..., string1() ] }
* TupleSpec = [ { Glyph, OptType, OptName } ]
* StringSpec = string()


Exports
-------

### egetopt:parse(Args, OptSpec) -> {ok, Plist, ArgsRemaining} | {error, Reason, Glyph}

Parse list of strings according to POSIX command-line option and argument rules.  An argument that starts with a leading hyphen (-) followed by a single character, "-f".  An option is either an option-flag, "-f", or option-parameter, "-x param".  Option-flags can appear together in any order as a list, "-hfg"; an option-parameter can appear at the end of list of option-flags, "-hfgx param" or "-hfgxparam".  Options can appear in any order and be repeated until a "--" argument is seen, which indicates the remainder are only arguments.

The `OptSpec` can be one of three formats:

* The `StringSpec`, similar to `getopt(3)`, is a string consisting of individual characters for option-flags and characters followed by a colon `:` to indicate an option-parameter is to follow.  As an extension, characters followed by a hash `#` for option-count and characters followed by semi-colon `;` for option-list.  The `OptName` used in the returned map is the option glyph prefixed by `opt_`.

* The `MapSpec` is a map keyed by option glyph, whos value is a tuple with `{OptName, OptType}`.

* The `TupleSpec` is a tuple list of `{Glyph, OptType, OptName}`.

The `Plist` returned is keyed by `OptName`.  Each option's value varies according to the `OptType` given by `OptSpec`: `flag` simple sets the option to `true`; `count` for the number of times an option flag is repeated; `param` sets an option to the last seen parameter string; and `list` set an option to a list of parameter strings collected in reverse order.

### egetopt:to_map(Args, OptSpec) -> {ok, Map, ArgsRemaining} | {error, Reason, Glyph}

Same as `egetopt:parse/2`, returning a `Map` in place of `Plist` for an `ok` result.

### opts:to_map(Args, OptSpec) -> {ok, Map, ArgsRemaining} | {error, Reason, Glyph}

Similar to `egetopt:to_map/2`, with the side-effect of saving the parsed options as an `ets` table.

### opts:get(OptName) -> Value | undefined

Get the `OptName` from the `ets` table returning a value.

### opts:get(OptName, Default) -> Value | Default

Get the `OptName` from the `ets` table returning a value; otherwise `Default`.

### opts:set(OptName, OptArg) -> true

Set the value of an `OptName` in the `ets` table.


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
$ example1 -aa -b -bb -cfoo -done -c replace -d two hey you
parsed #{opt_a => true,opt_b => 3,opt_c => "replace",
opt_d => ["two","one"]}, remaining ["hey","you"]

$ example1 -aa -b -bb -cfoo -done -- -c replace -d two hey you
parsed #{some_a => true,some_b => 3,some_c => "foo",some_d => ["one"]},
remaining ["-c", "replace", "-d", "two", "hey", "you"]

$ example1 -a -b -x -y
unknown option -x
usage: example [-a][-b...][-c param][-d item] ...
-a              set flag true
-b              count repeated occurences
-c param        set parameter
-d item         add item to list

$ example1 -a -c
missing parameter -c
usage: example [-a][-b...][-c param][-d item] ...
-a              set flag true
-b              count repeated occurences
-c param        set parameter
-d item         add item to list
```

`example1` uses a `getopt(3)` style option string specification with the extensions for count and list types.
`example2` uses the map specification with different option names.
`example3` uses the tuple-list specification with different option names.


Copyright
---------

Copyright 2017, 2022 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
