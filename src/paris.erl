-module(paris).

-export([
  main/1,
  get_version/0,
  get_script/0,
  get_script_name/0
  ]).
-include("paris.hrl").

main(Args) ->
  _ = application:start(paris),
  _ = paris_plugins:init(),
  run(Args).

run([]) ->
  help();
run(["--help"|Rest]) ->
  help(Rest);
run(["-h"|Rest]) ->
  help(Rest);
run(["--version"|_]) ->
  version();
run(["--rebar"|Rest]) ->
  paris_rebar:run(Rest);
run([Command|Params]) ->
  Module = command_module(Command),
  case eutils:module_exist(Module) of
    true ->
      Module:run(Params); 
    false ->
      ?CONSOLE("Command `~s' does not exist.", [Command]), help()
  end;
run(_) ->
  run([]).

get_script_name() ->
  filename:basename(escript:script_name()).

get_script() ->
  escript:script_name().

version() ->
  ?CONSOLE("~s ~s", [get_script_name(), get_version()]). 

get_version() ->
  case application:get_key(paris, vsn) of
    {ok, Vsn} -> Vsn;
    _ -> "0.0.0"
  end.

help() -> help([]).
help(Rest) ->
  version(),
  case Rest of
    [] ->
      ?CONSOLE("Usage : ~s [options] [commands]", [get_script_name()]),
      ?CONSOLE("~nOptions:~n", []),
      ?CONSOLE("-h   --help [command] : Display this help, or the specified command help", []),
      ?CONSOLE("     --version        : Display version", []),
      ?CONSOLE("     --rebar          : Run rebar command", []),
      ?CONSOLE("~nCommands:~n", []),
      ?CONSOLE("install               : Install components", []),
      ?CONSOLE("update                : Update paris.app", []),
      ?CONSOLE("new                   : Create a new Paris app", []),
      ?CONSOLE("generate              : Invoke a generator", []),
      ?CONSOLE("db                    : Manage database", []);
    [Command|_] ->
      Module = command_module(Command),
      try 
        Module:help()
      catch
        _:_ -> ?CONSOLE("Command `~s' does not exist!", [Command])
      end
  end.

command_module(Name) ->
  list_to_atom("paris_" ++ Name).

