-module(paris_pv_plugins_new).

-export([run/3]).

-define(PARAMS, [
                 {copyright_year, year()},
                 {author_name, "YOUR NAME"},
                 {author_email, "YOUR MAIL"},
                 {description, "A project build with Paris"}
                ]).

run(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help(Config);
    false -> create(Config, Args, Options)
  end.

%% private

create(Config, [AppName|Variables], Options) ->
  case paris_config:ready(Config) of
    true ->
      AppParams = paris_rebar:params(params(AppName, Variables)),
      paris_log:info("* Create app ~s", [AppName]),
      TemplateName = case elists:include(Options, 'without-texas') of
        true -> "paris";
        false -> case elists:include(Options, 'with-pg') of
            true -> "paris-pg";
            false -> case elists:include(Options, 'with-mysql') of
                true -> "paris-mysql";
                false -> "paris-sqlite"
              end
          end
      end,
      case elists:include(Options, 'force') of
        true -> paris_rebar:run(["-f", "create", "template=" ++ TemplateName] ++ AppParams);
        false -> paris_rebar:run(["create", "template=" ++ TemplateName] ++ AppParams)
      end;
    false ->
      paris_log:stop("! Please, run `~s update' to install templates", [paris:get_script_name()])
  end.

params(AppName, Variables) ->
  elists:merge_keylists(
    1, 
    [{name, eutils:to_string(AppName)}], 
    lists:foldl(fun(Var, Acc) ->
                    [Param|Value] = string:tokens(eutils:to_string(Var), "="),
                    Value1 = string:join(Value, "="),
                    elists:merge_keylists(1, [{eutils:to_atom(Param), Value1}], Acc)
                end, ?PARAMS, Variables)).

year() ->
  {{Y, _, _}, _} = calendar:local_time(),
  integer_to_list(Y).

help(Config) ->
  paris_plugins:help(new, Config, "new <app name>"),
  paris_log:print("~nParams:~n"),
  paris_log:print("copyright_year=<year> : Specify the copyright year (~s)", [year()]),
  paris_log:print("author_name=<name>    : Specify the author name"),
  paris_log:print("author_email=<mail>   : Specify the author mail"),
  paris_log:print("description=<desc>    : Project description").
