-module(paris_pv_plugins_update).

-export([run/3]).

run(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> paris_plugins:help(update, Config, "update");
    false -> update(Config, Args, Options)
  end.

%% private

update(Config, _Args, _Options) ->
  {URL, Branch} = case paris_config:git_template_url(Config) of
    X when is_tuple(X) -> X;
    Y when is_list(Y) -> {Y, "master"};
    _ -> paris_log:stop("! Invalid git url for templates")
  end,
  ParisCacheDir = paris_config:paris_cache_dir(Config),
  paris_log:info("* Clone ~s branch ~s", [URL, Branch]),
  case efile:remove_recursive(ParisCacheDir) of
    ok ->
      case git:download(URL, ParisCacheDir, {branch, [Branch]}) of
        {ok, _} ->
          paris_log:debug("update");
        {error, Reason} ->
          paris_log:stop("Can't clone template repository: ~p", [Reason])
      end;
    _ ->
      paris_log:stop("Can't remove ~s", [ParisCacheDir])
  end.
