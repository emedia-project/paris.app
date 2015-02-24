-module(paris_generator_model).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> help(Config);
    false -> g(Config, Options, Args)
  end.

generator_info() ->
  { model, "Generator to create model"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate model <params>").

g(_Config, _Options, _Args) ->
  paris_log:debug("* Generate model").
