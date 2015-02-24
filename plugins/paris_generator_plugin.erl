-module(paris_generator_plugin).

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
  {plugin, "Generator to create plugins"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate <generator> [params]").

g(Config, Options, [Name|_]) ->
  paris_log:debug("* Generate plugin ~s...", [Name]),
  PluginsPath = paris_config:paris_plugins_dir(Config),
  PluginModuleFile = filename:join([PluginsPath, "paris_plugins_" ++ eutils:to_string(Name) ++ ".erl"]),
  case paris_pv_plugins_generate:generate_status(PluginModuleFile, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [PluginModuleFile]);
    Status ->
      case paris_generator_plugin_dtl:render([{name, eutils:to_string(Name)}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), PluginModuleFile]),
          file:write_file(PluginModuleFile, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [PluginModuleFile])
      end
  end,
  PluginPPFile = filename:join([PluginsPath, "paris_plugins_" ++ eutils:to_string(Name) ++ ".pp"]),
  case paris_pv_plugins_generate:generate_status(PluginPPFile, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [PluginPPFile]);
    Status1 ->
      case paris_generator_plugin_pp_dtl:render([{name, eutils:to_string(Name)}]) of
        {ok, Data1} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status1)), PluginPPFile]),
          file:write_file(PluginPPFile, Data1);
        _ ->
          paris_log:stop("! Can't create ~s", [PluginPPFile])
      end
  end.

