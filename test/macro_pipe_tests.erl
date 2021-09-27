-module(macro_pipe_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("macro_pipe/include/macro_pipe.hrl").

-compile(no_auto_import).

ok_test() ->
  ?assertMatch(
    42,
    ?pipe([ ok ||
      {ok, Val} <- ok(42),
      Val
    ])
  ).

error_test() ->
  ?assertMatch(
    {error, non_42},
    ?pipe([ ok ||
      {ok, Val} <- error(non_42),
      Val
    ])
  ).

-define(error_else(Expr),
  ?assertMatch(
    {error, foo, non_42},
    ?pipe([ ok ||
      {ok, Val} <- Expr,
      Val
    ])
  )
).
error_else_test() ->
  ?error_else(error(non_42) or begin {error, Reason} = '_', {error, foo, Reason} end),
  ?error_else(error(non_42) or case '_' of {error, Reason} -> {error, foo, Reason} end),
  ?error_else(error(non_42) ?else({error, Reason}, {error, foo, Reason})).

simple_pipe_test() ->
  ?assertMatch(
    "hello",
    ?pipe([hello ||
      io_lib:format("~p", ['_']),
      lists:flatten('_')
    ])
  ).

case_in_pipe_test() ->
  ?assertMatch(
    long,
    ?pipe([hello ||
      io_lib:format("~p", ['_']),
      case lists:flatten('_') of
        List when erlang:length(List) > 1 ->
          long;
        _ ->
          short
      end
    ])
  ).

nested_pipe_test() ->
  ?assertMatch(
    "hello",
    ?pipe([hello ||
      io_lib:format("~p", ['_']),
      ?pipe(['_' ||
        lists:flatten('_')
      ])
    ])
  ).

ok(Var) ->
  {ok, Var}.

error(Reason) ->
  {error, Reason}.
