-module(macro_pipe).

-include_lib("astranaut/include/macro.hrl").
-include_lib("astranaut/include/quote.hrl").

-export_macro([pipe/1]).
-export([format_error/1]).

-type  svar() :: {Uniq::non_neg_integer(), Counter::non_neg_integer()}.
-type  tree() :: astranaut:tree().
-type trees() :: astranaut:trees().
-type error() :: 'bad pipe'.

-spec pipe(tree()) ->
  tree().
pipe({lc, _, Initial, Pipe}) ->
  SVar  = new_svar(erlang:phash2(erlang:make_ref(), 10000)),
  State = quote_svar(SVar),
  quote(
    begin
      _@State = _@Initial,
      unquote_splicing(pipe(Pipe, SVar))
    end
  );
pipe(_) ->
  {error, 'bad pipe'}.

-spec pipe(trees(), svar()) ->
  trees().
pipe([Head|Tail], SVar) ->
  NextSVar = next_svar(SVar),
  case Head of
    {generate, _, Pattern_, {op, _, 'or', Expr_, Else_}} ->
      Pattern = substitute_svar_or(quote_svar(NextSVar), Pattern_, quote_match_svar(NextSVar, Pattern_)),
      Expr    = substitute_svar(quote_svar(SVar    ), Expr_   ),
      Else    = substitute_svar(     quote(_Expr   ), Else_   ),
      [quote(
        case _@Expr of
          unquote = Pattern ->
            unquote_splicing(pipe(Tail, NextSVar));
          _Expr ->
            _@Else
        end
      )];
    {generate, _, Pattern_, Expr_} ->
      Pattern   = substitute_svar_or(quote_svar(NextSVar), Pattern_, quote_match_svar(NextSVar, Pattern_)),
      Expr      = substitute_svar(quote_svar(SVar), Expr_),
      [quote(
        case _@Expr of
          unquote = Pattern ->
            unquote_splicing(pipe(Tail, NextSVar));
          Expr ->
            Expr
        end
      )];
    {match, _, Pattern_, Expr_} ->
      Pattern   = substitute_svar_or(quote_svar(NextSVar), Pattern_, quote_match_svar(NextSVar, Pattern_)),
      Expr      = substitute_svar(quote_svar(SVar    ), Expr_   ),
      [
        quote(_@Pattern = _@Expr)
        | pipe(Tail, NextSVar)
      ];
    Expr_ ->
      Expr = substitute_svar(quote_svar(SVar), Expr_),
      [quote_match_svar(NextSVar, Expr) | pipe(Tail, NextSVar)]
  end;
pipe([], SVar) ->
  [quote_svar(SVar)].

-spec substitute_svar(tree(), tree()) ->
  tree().
substitute_svar(With, Where) ->
  astranaut:smap(
    fun
      (quote = '_') -> With;
      (__Form     ) -> __Form
    end,
    Where,
    #{}
  ).

-spec substitute_svar_or(tree(), tree(), tree()) ->
  tree().
substitute_svar_or(With, Where, Or) ->
  MapResults =
    astranaut:smapfold(
      fun
        (quote = '_', Counter) -> {With  , Counter + 1};
        (__Form     , Counter) -> {__Form, Counter    }
      end,
      0,
      Where,
      #{}
    ),
  case MapResults of
    {_      , 0} -> Or;
    {Results, _} -> Results
  end.

-spec quote_match_svar(svar(), tree()) ->
  tree().
quote_match_svar(SVar, Expr) ->
  State = quote_svar(SVar),
  quote(_@State = _@Expr).

%%

-spec new_svar(non_neg_integer()) ->
  svar().
new_svar(Uniq) ->
  {0, Uniq}.

-spec next_svar(svar()) ->
  svar().
next_svar({N, Uniq}) ->
  {N + 1, Uniq}.

-spec quote_svar(svar()) ->
  tree().
quote_svar({N, Uniq}) ->
  State = list_to_atom("_S" ++ integer_to_list(Uniq) ++ integer_to_list(N)),
  quote(_V@State).

%%

-spec format_error(error()) ->
  string().
format_error('bad pipe') ->
  "pipe argument must be a list comprehension (e.g. [hello || ok <- io_lib:format(\"~p\", ['_'])])";
format_error(Other) ->
  io_lib:format("unknown error ~p", [Other]).
