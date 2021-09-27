-ifndef(macro_pipe).
-define(macro_pipe, yeah).

-ifdef(macro_pipe_debug).
  -define(macro_pipe_debug_, debug).
-else.
  -define(macro_pipe_debug_, ).
-endif.

-include_lib("astranaut/include/macro.hrl").
-import_macro(macro_pipe).
-use_macro({macro_pipe, pipe/1, [?macro_pipe_debug_]}).
-define(pipe(Pipe), macro_pipe:pipe(Pipe)).
-define(else(Pattern, Expr), or begin Pattern = '_', Expr end).

-endif.
