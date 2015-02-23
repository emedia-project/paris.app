-module(paris_pv_plugins_generate).

-export([run/3]).

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

help(Config) ->
  paris_plugins:help(generate, Config, "generate <params>").

