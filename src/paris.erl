-module(paris).

-export([
         main/1,
         get_script_name/0,
         get_script/0,
         get_version/0
        ]).
-include("paris.hrl").

main(Args) ->
  application:start(?MODULE),
  Config = paris_config:load(),
  Plugins = maps:merge(paris_plugins:find(Config), ?PV_PLUGINS),
  Config1 = paris_config:plugins(Config, Plugins),
  case getopt:parse(paris_config:options(Config1) ++ opts(), Args) of
    {ok, {Options, []}} ->
      case lists:member(version, Options) of
        true ->
          application:load(paris),
          {ok, Vsn} = application:get_key(paris, vsn),
          io:format("paris ~s~n", [Vsn]);
        false ->
          help(Config1)
      end;
    {ok, {Options, Commands}} ->
      run(Config1, Options, lists:map(fun eutils:to_atom/1, Commands));
    {error, _Details} ->
      help(Config1)
  end.

get_script_name() ->
  filename:basename(get_script()).

get_script() ->
  escript:script_name().

get_version() ->
  {ok, Vsn} = application:get_key(paris, vsn),
  Vsn.

%% private

run(Config, Options, [Command | Args]) ->
  call(Config, Command, Options, Args).

call(Config, Command, Options, Args) ->
  case paris_config:plugin_module(Command, Config) of
    #{module := Module} -> 
      Module:run(Config, Options, Args);
    _ ->
      ?HALT("! Invalid command: ~s", [Command])
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

