-module(paris_pv_plugins_commands).

-export([run/3]).

run(Config, _Options, _Args) ->
  Plugins = paris_config:plugins(Config),
  lists:foreach(fun(Command) ->
                    case maps:get(Command, Plugins, undefined) of
                      #{command := Cmd, 
                        depends := [], 
                        desc := Desc} ->
                        io:format("~s: ~s~n", [Cmd, Desc]);
                      #{command := Cmd, 
                        depends := Deps, 
                        desc := Desc} ->
                        io:format("~s: ~s ~p~n", [Cmd, Desc, Deps]);
                      _ -> ok
                    end
                end, maps:keys(Plugins)).

