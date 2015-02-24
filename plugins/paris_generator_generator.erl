-module(paris_generator_generator).

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
  {generator, "Generator to create generator"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate generator <name>").

g(Config, Options, [Name|_]) ->
  paris_log:info("* Generate generator ~s...", [Name]),
  PluginsPath = paris_config:paris_plugins_dir(Config),
  GeneratorModuleFile = filename:join([PluginsPath, "paris_generator_" ++ eutils:to_string(Name) ++ ".erl"]),
  case paris_pv_plugins_generate:generate_status(GeneratorModuleFile, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [GeneratorModuleFile]);
    Status ->
      case paris_generator_generator_dtl:render([{name, eutils:to_string(Name)}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), GeneratorModuleFile]),
          file:write_file(GeneratorModuleFile, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [GeneratorModuleFile])
      end
  end.

