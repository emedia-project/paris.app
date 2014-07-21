-module(paris_generator_plugin).

-export([generate/1]).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

generate(Params) ->
  PluginsPath = paris_plugins:path(),
  case paris_utils:make_dir(PluginsPath) of
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
  ?CONSOLE("Generate plugin generator ~s", [PluginFile]),
  file:write_file(PluginFile, 
  "-module(paris_generator_" ++ Name ++ ").
  
-export([generate/1]).
-define(CONSOLE(Str, Args), io:format(Str++\"\~n\", Args)).

generate([\"--help\"|_]) -> help();
generate(_Params) ->
  ?CONSOLE(\"Execute generator `" ++ Name ++ "'...\", []).

help() ->
  ?CONSOLE(\"Usage:\", []),
  ?CONSOLE(\"\~s generate " ++ Name ++ " [params] [options]\", [paris:get_script_name()]),
  ?CONSOLE(\"\~nOptions:\~n\", []),
  ?CONSOLE(\"     --help           : Display this help\", []).");
g(PluginsPath, [Name|_]) ->
  PluginFile = filename:join([PluginsPath, "paris_" ++ Name ++ ".erl"]),
  ?CONSOLE("Generate plugin ~s", [PluginFile]),
  file:write_file(PluginFile, 
  "-module(paris_" ++ Name ++ ").
  
-export([run/1, help/0]).
-define(CONSOLE(Str, Args), io:format(Str++\"\~n\", Args)).

run(_Params) ->
  ?CONSOLE(\"Execute command `" ++ Name ++ "'...\", []).

help() ->
  ?CONSOLE(\"Usage:\", []),
  ?CONSOLE(\"\~s " ++ Name ++ " [params] [options]\", [paris:get_script_name()]).").

