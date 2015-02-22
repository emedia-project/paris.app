-module(paris).

-export([
         main/1,
         get_script_name/0
        ]).
-include("paris.hrl").

main(Args) ->
  Plugins = maps:merge(paris_plugins:find(), ?PV_PLUGINS),
  Config = paris_config:load(Plugins),
  case getopt:parse(paris_config:options(Config) ++ opts(), Args) of
    {ok, {Options, []}} ->
      case lists:member(version, Options) of
        true ->
          application:load(paris),
          {ok, Vsn} = application:get_key(paris, vsn),
          io:format("paris ~s~n", [Vsn]);
        false ->
          help(Config)
      end;
    {ok, {Options, Commands}} ->
      run(Config, Options, lists:map(fun eutils:to_atom/1, Commands));
    {error, _Details} ->
      help(Config)
  end.

get_script_name() ->
  filename:basename(escript:script_name()).

%% private

run(Config, Options, [Command | Args]) ->
  call(Config, Command, Options, Args).

call(Config, Command, Options, Args) ->
  case paris_config:plugin_module(Command, Config) of
    #{module := Module} -> 
      Module:run(Config, Options, Args);
    X ->
      ?HALT("Invalid command: ~s: ~p", [Command, X])
  end.

opts() ->
  [
   {help,     $h, "help",     undefined, "Display this help"},
   {version,  $V, "version",  undefined, "Display version"}
  ].

help(Config) -> help(Config, "<command>").
help(Config, Msg) ->
  getopt:usage(opts(), "paris", Msg),
  io:format("Commandes:~n~n"),
  run(Config, [], [commands]),
  stop.

