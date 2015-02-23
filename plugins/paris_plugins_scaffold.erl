-module(paris_plugins_scaffold).

-export([run/3]).

run(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> help(Config);
    false -> scaffold(Config, Args, Options)
  end.

%% Private

scaffold(_Config, _Args, _Options) ->
  paris_log:debug("Execute plugin scaffold").

help(Config) ->
  paris_plugins:help(scaffold, Config, "scaffold").
