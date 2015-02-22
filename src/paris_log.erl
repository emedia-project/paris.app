-module(paris_log).

-export([
         print/2,
         info/2,
         debug/2,
         err/2,
         stop/2,
         print/1,
         info/1,
         debug/1,
         err/1,
         stop/1
        ]).

-include("paris.hrl").

print(Fmt, Args) -> ?PRINT(Fmt, Args).
info(Fmt, Args) -> ?INFO(Fmt, Args).
debug(Fmt, Args) -> ?DEBUG(Fmt, Args).
err(Fmt, Args) -> ?ERROR(Fmt, Args).
stop(Fmt, Args) -> ?HALT(Fmt, Args).
print(Fmt) -> ?PRINT(Fmt, []).
info(Fmt) -> ?INFO(Fmt, []).
debug(Fmt) -> ?DEBUG(Fmt, []).
err(Fmt) -> ?ERROR(Fmt, []).
stop(Fmt) -> ?HALT(Fmt, []).
