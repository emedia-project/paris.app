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
        case filename:extension(File) of
          ".erl" ->
            _ = case compile:file(File, [{outdir, PluginsPath}]) of
              error -> ?CONSOLE("Failed to load plugin from ~s", [File]);
              {error, _, _} -> ?CONSOLE("Failed to load plugin from ~s", [File]);
              _ -> ok
            end;
          ".dtl" ->
            Module = list_to_atom(filename:basename(File, ".dtl") ++ "_dtl"),
            _ = case erlydtl:compile_file(File, Module, [{out_dir, PluginsPath}]) of
              error -> ?CONSOLE("Failed to load template ~s", [File]);
              {error, E, _} -> ?CONSOLE("Failed to load template ~s : ~p", [File, E]);
              _ -> ok
            end;
          _ -> ok
        end
    end, filelib:wildcard(filename:join([PluginsPath, "*"]))).
