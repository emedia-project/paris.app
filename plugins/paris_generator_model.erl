-module(paris_generator_model).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> help();
    false -> g(Config, Args)
  end.

generator_info() ->
  { model, "Generator to create model"}.

help() ->
  paris_log:print("Usage:"),
  paris_log:print("~s generate model <name> [options]", [paris:get_script_name()]),
  paris_log:print("~nOptions:~n"),
  paris_log:print("     --help           : Display this help").

g(_Config, _Args) ->
  paris_log:debug("* Generate model").
