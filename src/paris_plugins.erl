-module(paris_plugins).

-export([
  init/0,
  path/0
  ]).
-include("paris.hrl").

init() ->
  PluginsPath = path(),
  _ = case filelib:is_dir(PluginsPath) of
    true -> init_plugins(PluginsPath);
    false -> ok
  end.

path() ->
  filename:join([os:getenv("HOME"), ".paris", "plugins"]).

init_plugins(PluginsPath) ->
  code:add_patha(PluginsPath),
  lists:foreach(fun(File) ->
        _ = case compile:file(File, [{outdir, PluginsPath}]) of
          error -> ?CONSOLE("Failed to load plugin from ~s", [File]);
          {error, _, _} -> ?CONSOLE("Failed to load plugin from ~s", [File]);
          _ -> ok
        end
    end, filelib:wildcard(filename:join([PluginsPath, "*.erl"]))).
