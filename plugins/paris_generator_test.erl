-module(paris_generator_test).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help(Config);
    false -> g(Config, Options, Args)
  end.

generator_info() ->
  { test, "Generator to create test"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate test <name>").

g(_Config, Options, [Name|_]) ->
  Name1 = inflector:underscore(eutils:to_string(Name)),
  case paris_app:is_app() of
    false ->
      paris_log:stop("! Not a paris application");
    {true, [AppName]} ->
      paris_log:info("* Create test ~s", [Name1]),
      File = filename:join(["test", Name1 ++ "_tests.erl"]),
      case paris_pv_plugins_generate:generate_status(File, Options) of
        skip ->
          paris_log:debug("* Skip file ~s: already exist", [File]);
        Status ->
          case paris_generator_test_dtl:render([{appname, AppName}, {name, Name}]) of
            {ok, Data} ->
              paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
              file:write_file(File, Data);
            _ ->
              paris_log:stop("! Can't create ~s", [File])
          end
      end;
    {true, _} ->
      paris_log:stop("! Can't create controller in a multi application")
  end.
