-module(paris_generator_plugin).

-export([generate/1]).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

generate(Params) ->
  PluginsPath = paris_plugins:path(),
  case efile:make_dir(PluginsPath) of
    {error, _} -> io:format("[E] Can't create plugin directory~n");
    ok -> g(PluginsPath, Params)
  end.

g(_, ["--help"|_]) ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s generate plugin [params] [options]", [paris:get_script_name()]),
  ?CONSOLE("~nParams:~n", []),
  ?CONSOLE("<name>            : Generate a command plugin with <name>", []),
  ?CONSOLE("generator <name>  : Generate a generator plugin with <name>", []),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --help           : Display this help", []);
g(PluginsPath, ["generator", Name|_]) ->
  PluginFile = filename:join([PluginsPath, "paris_generator_" ++ Name ++ ".erl"]),
  case plugin_generator_dtl:render([{name, Name}]) of
    {ok, Data} -> 
      ?CONSOLE("Generate plugin generator ~s", [PluginFile]),
      file:write_file(PluginFile, Data);
    _ -> ?CONSOLE("Faild to generate plugin ~s", [PluginFile])
  end;
g(PluginsPath, [Name|_]) ->
  PluginFile = filename:join([PluginsPath, "paris_" ++ Name ++ ".erl"]),
  case plugin_command_dtl:render([{name, Name}]) of
    {ok, Data} -> 
      ?CONSOLE("Generate plugin ~s", [PluginFile]),
      file:write_file(PluginFile, Data);
    _ -> ?CONSOLE("Faild to generate plugin ~s", [PluginFile])
  end.

