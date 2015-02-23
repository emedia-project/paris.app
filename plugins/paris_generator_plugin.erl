-module(paris_generator_plugin).

-export([generate/3]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help();
    false -> g(Config, Args)
  end.

help() ->
  paris_log:print("Usage:"),
  paris_log:print("~s generate plugin <name> [options]", [paris:get_script_name()]),
  paris_log:print("~nOptions:~n"),
  paris_log:print("     --help           : Display this help").

g(_Config, [Name|_]) ->
  paris_log:debug("* Generate plugin ~s...", [Name]),
  PluginsPath = paris_config:paris_plugins_dir(),
  PluginModuleFile = filename:join([PluginsPath, "paris_plugins_" ++ eutils:to_string(Name) ++ ".erl"]),
  case paris_generator_plugin_dtl:render([{name, eutils:to_string(Name)}]) of
    {ok, Data} ->
      paris_log:info("* Generate ~s", [PluginModuleFile]),
      file:write_file(PluginModuleFile, Data);
    _ ->
      paris_log:stop("! Can't create ~s", [PluginModuleFile])
  end,
  PluginPPFile = filename:join([PluginsPath, "paris_plugins_" ++ eutils:to_string(Name) ++ ".pp"]),
  case paris_generator_plugin_pp_dtl:render([{name, eutils:to_string(Name)}]) of
    {ok, Data1} ->
      paris_log:info("* Generate ~s", [PluginPPFile]),
      file:write_file(PluginPPFile, Data1);
    _ ->
      paris_log:stop("! Can't create ~s", [PluginPPFile])
  end.

