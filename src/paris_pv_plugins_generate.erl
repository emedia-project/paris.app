-module(paris_pv_plugins_generate).

-export([
         run/3,
         generate_status/2
        ]).

run(Config, Options, Args) ->
  case (Args =:= []) of
    true -> help(Config);
    false ->
      [Command|Args1] = Args,
      Module = eutils:to_atom("paris_generator_" ++ eutils:to_string(Command)),
      case eutils:module_exist(Module) of
        true ->
          Module:generate(Config, Options, Args1);
        false ->
          paris_log:err("! Generator `~s' does not exist.", [Command])
      end
  end.

generate_status(File, Options) ->
  case filelib:is_file(File) of
    true ->
      case elists:include(Options, force) of
        true -> overwrite ;
        false -> skip
      end;
    false -> create
  end.

help(Config) ->
  paris_plugins:help(generate, Config, "generate <params>"),
  PluginsPath = paris_config:paris_plugins_dir(Config),
  paris_log:print("Generators:~n"),
  lists:foreach(fun(File) ->
                    Module = eutils:to_atom(filename:basename(File, ".beam")),
                    case code:ensure_loaded(Module) of
                      {module, Module} ->
                        case erlang:function_exported(Module, generator_info, 0) of
                          true -> 
                            case Module:generator_info() of
                              {Generator, Desc} ->
                                paris_log:print("~s: ~s", [Generator, Desc]);
                              _ ->
                                ok
                            end;
                          false -> ok
                        end;
                      _ ->
                        ok
                    end
                end, filelib:wildcard(filename:join([PluginsPath, "paris_generator_*.beam"]))).


