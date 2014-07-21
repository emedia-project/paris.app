-module(paris_new).

-export([run/1, help/0]).
-include("paris.hrl").

run([AppName|Params]) ->
  create(AppName, Params).

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s new <app_name> [params] [options]", [paris:get_script_name()]),
  ?CONSOLE("~nParams:~n", []),
  ?CONSOLE("copyright_year=<year> : Specify the copyright year (~s)", [year()]),
  ?CONSOLE("author_name=<name>    : Specify the author name", []),
  ?CONSOLE("author_email=<mail>   : Specify the author mail", []),
  ?CONSOLE("description=<desc>    : Project description", []),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("-f   --force          : Force overwriting", []),
  ?CONSOLE("     --without-texas  : Create an application without using Texas", []),
  ?CONSOLE("     --with-pg        : Use PostgreSQL", []),
  ?CONSOLE("     --with-mysql     : Use MySQL", []),
  ?CONSOLE("     --with-sqlite    : Use SQLite (default)", []).

create(AppName, Params) ->
  AppParams = paris_rebar:build_params(Params, [
        {name, AppName}, 
        {copyright_year, year()},
        {author_name, "YOUR_NAME"},
        {author_email,"YOUR_MAIL"},
        {description, "A project build with Paris"}
        ]),
  ?CONSOLE("Create app ~s...", [AppName]),
  TemplateName = case paris_utils:include("--without-texas", Params) of
    true -> "paris";
    false -> case paris_utils:include("--with-pg", Params) of
        true -> "paris-pg";
        false -> case paris_utils:include("--with-mysql", Params) of
            true -> "paris-mysql";
            false -> "paris-sqlite"
          end
      end
  end,
  case paris_utils:include("--force", Params) or paris_utils:include("-f", Params) of
    true -> paris_rebar:run(["-f", "create", "template=" ++ TemplateName] ++ AppParams);
    false -> paris_rebar:run(["create", "template=" ++ TemplateName] ++ AppParams)
  end.

year() ->
  {{Y, _, _}, _} = calendar:local_time(),
  integer_to_list(Y).
