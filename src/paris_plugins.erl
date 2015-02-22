-module(paris_plugins).

-export([
         find/0,
         help/3
        ]).
-include("paris.hrl").

find() ->
  PluginsPath = path(),
  _ = case filelib:is_dir(PluginsPath) of
        true -> load(PluginsPath);
        false -> []
      end.

help(Command, Config, Info) ->
  getopt:usage(paris_config:options(Command, Config), "paris", Info).

path() ->
  filename:join([os:getenv("HOME"), ".paris", "plugins"]).

load(PluginsPath) ->
  code:add_patha(PluginsPath),
  lists:foldl(fun(File, Plugins) ->
        case filename:extension(File) of
          ".erl" ->
            case compile:file(File, [{outdir, PluginsPath}]) of
              error -> ?HALT("Failed to load plugin from ~s", [File]);
              {error, _, _} -> ?HALT("Failed to load plugin from ~s", [File]);
              _ -> Plugins
            end;
          ".dtl" ->
            Module = list_to_atom(filename:basename(File, ".dtl") ++ "_dtl"),
            case erlydtl:compile_file(File, Module, [{out_dir, PluginsPath}]) of
              error -> ?HALT("Failed to load template ~s", [File]);
              {error, E, _} -> ?HALT("Failed to load template ~s : ~p", [File, E]);
              _ -> Plugins
            end;
          ".pp" ->
            #{command := Command} = PluginMap = maps:from_list(file:consult(File)),
            maps:put(Command, PluginMap, Plugins);
          _ -> Plugins
        end
    end, #{}, filelib:wildcard(filename:join([PluginsPath, "*"]))).
