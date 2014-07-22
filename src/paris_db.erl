-module(paris_db).

-export([run/1, help/0]).
-include("paris.hrl").

run([Action|_]) ->
  case paris_app:is_app() of
    {true, [AppName]} -> 
      paris_rebar:run(["db-" ++ Action, "texas=config/" ++ AppName ++ ".config"]);
    _ -> ?CONSOLE("[E] Not a paris application!", [])
  end.

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s db <action>", [paris:get_script_name()]),
  ?CONSOLE("~nActions:~n", []),
  ?CONSOLE("create     : Create the database", []),
  ?CONSOLE("drop       : Drop the database", []),
  ?CONSOLE("seed       : Seed the database", []).

