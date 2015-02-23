-module(paris_generator_generator).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help();
    false -> g(Config, Args)
  end.

generator_info() ->
  {generator, "Generator to create generator"}.

help() ->
  paris_log:print("Usage:"),
  paris_log:print("~s generate generator <name> [options]", [paris:get_script_name()]),
  paris_log:print("~nOptions:~n"),
  paris_log:print("     --help           : Display this help").

g(Config, [Name|_]) ->
  paris_log:info("* Generate generator ~s...", [Name]),
  PluginsPath = paris_config:paris_plugins_dir(Config),
  GeneratorModuleFile = filename:join([PluginsPath, "paris_generator_" ++ eutils:to_string(Name) ++ ".erl"]),
  case paris_generator_generator_dtl:render([{name, eutils:to_string(Name)}]) of
    {ok, Data} ->
      paris_log:info("* Generate ~s", [GeneratorModuleFile]),
      file:write_file(GeneratorModuleFile, Data);
    _ ->
      paris_log:stop("! Can't create ~s", [GeneratorModuleFile])
  end.

