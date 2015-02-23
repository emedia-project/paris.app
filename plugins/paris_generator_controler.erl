-module(paris_generator_controler).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help();
    false -> g(Config, Args)
  end.

generator_info() ->
  { controler, "Generator to create controler"}.

help() ->
  paris_log:print("Usage:"),
  paris_log:print("~s generate controler <name> [options]", [paris:get_script_name()]),
  paris_log:print("~nOptions:~n"),
  paris_log:print("     --help           : Display this help").

g(_Config, [Name|_Args]) ->
  paris_log:info("* Generate controler ~s", [Name]).
