-module(paris_generate).

-export([run/1, help/0]).
-include("paris.hrl").

run([Generator|Params]) ->
  generate(Generator, Params).

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s generate <generator> [params] [options]", [paris:get_script_name()]).

generate(Generator, Params) ->
  Module = generator_module(Generator),
  case paris_utils:module_exist(Module) of
    true ->
      Module:generate(Params);
    false ->
      ?CONSOLE("Generator `~s' does not exist.", [Generator])
  end.

generator_module(Name) ->
  list_to_atom("paris_generator_" ++ Name).
