-module(paris_plugins_clean).

-export([run/3]).

run(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> help(Config);
    false -> clean(Config, Args, Options)
  end.

%% Private

clean(Config, _Args, _Options) ->
  ParisDir = paris_config:paris_dir(Config),
  TemplateDir = paris_config:template_dir(Config),
  paris_log:info("* Remove ~s", [ParisDir]),
  efile:remove_recursive(ParisDir),
  lists:foreach(fun(File) ->
                    paris_log:info("* Remove ~s", [File]),
                    efile:remove_recursive(File)
                end, filelib:wildcard(filename:join([TemplateDir, "paris*"]))).

help(Config) ->
  paris_plugins:help(clean, Config, "clean").
