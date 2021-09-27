# Macro pipe

This is a small library that provide macros that helps write code that can be expressed as a pipe.
Built up on [astranaut](https://github.com/slepher/astranaut) metaprogramming library.

## Usage

To use import `macro_pipe.hrl` file and then use macro `?pipe`.

```erlang
-module(macro_pipe_examples).
-include_lib("macro_pipe/include/macro_pipe.hrl").

-export([print_hello/0).

print_hello() ->
  ?pipe([hello ||
    io_lib:format("~p", ['_']),
    lists:flatten('_')
  ]).
```

## Examples

There are several examples showing the functionality and base use cases.

Simple pipe where `'_'` replaced with current state variable.
```erlang
?pipe([hello ||
  io_lib:format("~p", ['_']),
  lists:flatten('_')
]).

=>

S0 = hello,
S1 = io_lib:format("~p", [_S1010]),
S2 = lists:flatten(_S1011),
S2

=>

1> macro_pipe_examples:print_hello().
"hello"

```

More complicated pipe with filtering and error handling.

```erlang
?pipe([ok ||
  {ok, '_'} <- io:fread("filename: ", "~s"),
  {ok, '_'} <- file:read_file('_'),
   ok       <- io:fwrite("~s~n", ['_'])
]).

=>

case io:fread("filename: ", "~s") of
  {ok, S1} ->
    case file:read_file(S1) of
      {ok, S2} ->
        case io:fwrite("~s~n", [S2]) of
          S3 = ok -> S3;
          Other -> Other
        end;
      Other ->
        Other
    end;
  Other ->
    Other
end

=>

1> macro_pipe_examples:print_file().
filename: rebar.lock
{"1.2.0",
[{<<"astranaut">>,{pkg,<<"astranaut">>,<<"0.10.1">>},0}]}.
[
{pkg_hash,[
 {<<"astranaut">>, <<"766B70C82B6C68BDA62B3878642B2CE248EFF05187240A32105E41C68FD1390E">>}]},
{pkg_hash_ext,[
 {<<"astranaut">>, <<"515472F5666AF5EAB72C6A7FD914CE4BCBD7F8020A74916F616729008393FCFF">>}]}
].

ok

2> macro_pipe_examples:print_file().
filename: bad_file
{error,enoent}
```


Even more complicated pipe with error mapping.
```erlang
  ?pipe([ok ||
    {ok, Filename} <- io:fread("filename: ", "~s")          ?else({error, Reason}, {error, {'failed to read filename', Reason}}),
     Basename      =  filename:basename(Filename),
     Type          =  filename:extension(Filename),
    {_, Processor} <- lists:keyfind(Type, 1, Processors)    or {error, {'processor for such file type not found', Type}},
    {ok, File}     <- file:read_file(Filename)              ?else({error, Reason}, {error, {'failed to read file', Filename, Reason}}),
    {ok, NewFile}  <- Processor(File)                       ?else({error, Reason}, {error, {'failed to process file', Filename, Reason}}),
     NewFilename   =  lists:flatten(Basename ++ ".foo"),
     ok            <- file:write_file(NewFilename, NewFile) ?else({error, Reason}, {error, {'failed to writing file', NewFilename, Reason}}),
    {ok, NewFilename}
  ]).

=>

case io:fread("filename: ", "~s") of
  {ok, Filename} ->
    Basename = filename:basename(Filename),
    Type = filename:extension(Filename),
    case lists:keyfind(Type, 1, Processors) of
      {_, Processor} ->
        case file:read_file(Filename) of
          {ok, File} ->
            case Processor(File) of
              {ok, NewFile} ->
                NewFilename = lists:flatten(Basename ++ ".foo"),
                case file:write_file(NewFilename, File) of
                  ok ->
                    {ok, NewFilename};
                  Other ->
                    {error, Reason} = Other,
                    {error, {'failed to writing file', NewFilename, Reason}}
                end;
              Other ->
                {error, Reason} = Other,
                {error, {'failed to process file', Filename, Reason}}
            end;
          Other ->
            {error, Reason} = Other,
            {error, {'failed to read file', Filename, Reason}}
        end;
      Other ->
        false = Other,
        {error, {'processor for such file type not found', Type}}
    end;
  Other ->
    {error, Reason} = Other,
    {error, {'failed to read filename', Reason}}
end

=>

1> macro_pipe_examples:process_file([{".lock", fun(V) -> {ok, V} end}]).
filename: rebar.lock
{ok,"rebar.lock.foo"}

2> file:read_file("rebar.lock.foo").
{ok,<<"{\"1.2.0\",\n[{<<\"astranaut\">>,{pkg,<<\"astranaut\">>,<<\"0.10.1\">>},0}]}.\n[\n{pkg_hash,[\n {<<\"astranaut\">>, <<\"766"...>>}

3> macro_pipe_examples:process_file([]).
filename: rebar.lock
{error,{'processor for such file type not found',".lock"}}

```

Notice that `?else` macro base on `or` keyword, so collision may happen.

## Debug

To debug macros expanding define `macro_pipe_debug` macro before including `macro_pipe.hrl` file, e.g. in rebar.config

```erlang
{erl_opts, [
  {d, macro_pipe_debug}
]}.
```
