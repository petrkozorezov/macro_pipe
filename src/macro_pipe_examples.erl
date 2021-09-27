-module(macro_pipe_examples).
-include_lib("macro_pipe/include/macro_pipe.hrl").

-export([print_hello/0, print_file/0, process_file/1]).

print_hello() ->
  ?pipe([hello ||
    io_lib:format("~p", ['_']),
    lists:flatten('_')
  ]).

print_file() ->
  ?pipe([ok ||
    {ok, '_'} <- io:fread("filename: ", "~s"),
    {ok, '_'} <- file:read_file('_'),
     ok       =  io:fwrite("~s~n", ['_'])
  ]).

-type processor() :: fun((binary()) -> {ok, binary()} | {error, term()}).
-spec process_file([{Filetype::string(), processor()}]) ->
  {ok, NewFile::string()} | {error, term()}.
process_file(Processors) ->
  ?pipe([ok ||
    {ok, Filename}    <- io:fread("filename: ", "~s")          ?else({error, Reason}, {error, {'failed to read filename', Reason}}),
    Basename          =  filename:basename(Filename),
    Type              =  filename:extension(Filename),
    {_, Processor}    <- lists:keyfind(Type, 1, Processors)    or {error, {'processor for such file type not found', Type}},
    {ok, File}        <- file:read_file(Filename)              ?else({error, Reason}, {error, {'failed to read file', Filename, Reason}}),
    {ok, NewFile}     <- Processor(File)                       ?else({error, Reason}, {error, {'failed to process file', Filename, Reason}}),
    NewFilename       =  lists:flatten(Basename ++ ".foo"),
     ok               <- file:write_file(NewFilename, NewFile) ?else({error, Reason}, {error, {'failed to writing file', NewFilename, Reason}}),
    {ok, NewFilename}
  ]).
