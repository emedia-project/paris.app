-module(paris_plugins).

-export([
         find/1,
         help/3
        ]).
-include("paris.hrl").

find(Config) ->
  PluginsPath = paris_config:paris_plugins_dir(Config),
  _ = case filelib:is_dir(PluginsPath) of
        true -> load(PluginsPath);
        false -> #{}
      end.

help(Command, Config, Info) ->
  getopt:usage(paris_config:options(Command, Config), "paris", Info).

% Private 

load(PluginsPath) ->
  code:add_patha(PluginsPath),
  lists:foldl(fun(File, Plugins) ->
        case filename:extension(File) of
          ".erl" ->
            case compile:file(File, [{outdir, PluginsPath}]) of
              error -> ?HALT("Failed to load plugin from ~s", [File]);
              {error, _, _} -> ?HALT("Failed to compile plugin file ~s", [File]);
              _ -> Plugins
            end;
          ".dtl" ->
            Module = list_to_atom(filename:basename(File, ".dtl") ++ "_dtl"),
            case erlydtl:compile_file(File, Module, [{out_dir, PluginsPath}]) of
              error -> ?HALT("Failed to load template ~s", [File]);
              {error, E, _} -> ?HALT("Failed to compile plugin file ~s : ~p", [File, E]);
              _ -> Plugins
            end;
          ".pp" ->
            case file:consult(File) of
              {ok, PluginData} ->
                #{command := Command} = PluginMap = maps:from_list(PluginData),
                maps:put(Command, PluginMap, Plugins);
              _ ->
                ?HALT("Can read plugin file ~s", [File])
            end;
          _ -> Plugins
        end
    end, #{}, filelib:wildcard(filename:join([PluginsPath, "*"]))).
