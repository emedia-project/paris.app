-module(paris_pv_plugins_update).

-export([run/3]).

-define(TEMPLATES_DIR, "rebar_templates").
-define(PLUGINS_DIR, "plugins").

run(Config, Options, Args) ->
  case (elists:include(Options, help)) of
    true -> paris_plugins:help(update, Config, "update");
    false -> update(Config, Args, Options)
  end.

%% private

update(Config, _Args, Options) ->
  URL = paris_config:git_template_url(Config),
  ParisCacheDir = paris_config:paris_cache_dir(Config),
  RebarTemplateDir = paris_config:template_dir(Config),
  ParisPluginsDir = paris_config:paris_plugins_dir(Config),
  paris_log:info("* Update versions list from ~s", [URL]),
  case efile:remove_recursive(ParisCacheDir) of
    {error, E} when E =:= enoent ->
      ok;
    ok ->
      ok;
    _ ->
      paris_log:stop("Can't remove ~s", [ParisCacheDir])
  end,
  case git:clone(URL, ParisCacheDir) of
    {ok, _} ->
      case 
        elists:include(Options, force) orelse
        elists:keysearch(fun({branch, _}) -> true;
                            (_) -> false
                         end, Options, false) =/= false orelse
        need_update(ParisCacheDir, RebarTemplateDir)
      of
        true ->
          perform_update(Options, ParisCacheDir, RebarTemplateDir, ParisPluginsDir);
        false ->
          paris_log:info("* Already up-to-date")
      end;
    {error, Reason} ->
      paris_log:stop("Can't clone template repository: ~p", [Reason])
  end.

perform_update(Options, ParisCacheDir, RebarTemplateDir, ParisPluginsDir) ->
  Version = case elists:keysearch(fun({branch, _}) -> true;
                                     (_) -> false
                                  end, Options) of
              {branch, Branch} -> Branch;
              _ -> case elists:fmax(fun semver:from_str/1, git:tags(ParisCacheDir)) of
                     error -> 
                       paris_log:stop("! Can't find valid version");
                     V -> V
                   end
            end,
  paris_log:info("* Update to version ~s", [Version]),
  case git:checkout(ParisCacheDir, Version) of
    {ok, []} ->
      _ = update_templates(Options, ParisCacheDir, RebarTemplateDir),
      _ = update_plugins(Options, ParisCacheDir, ParisPluginsDir),
      _ = update_paris(ParisCacheDir);
    _ ->
      paris_log:stop("! Can't find valid version ~s", [Version])
  end.

update_templates(Options, ParisCacheDir, RebarTemplateDir) ->
  case elists:include(Options, templates) orelse not elists:include(Options, plugins) of
    true ->
      paris_log:info("* Update templates..."),
      case file:list_dir_all(filename:join([ParisCacheDir, ?TEMPLATES_DIR])) of
        {ok, Files} ->
          lists:foreach(fun(File) ->
                            case file:delete(File) of
                              ok ->
                                paris_log:info("* Remove old file ~s", [filename:basename(File)]);
                              _ ->
                                paris_log:err("! Can't remove file ~s", [filename:basename(File)])
                            end
                        end, filelib:wildcard(filename:join([RebarTemplateDir, "paris*"]))),
          lists:foreach(fun(File) ->
                            case file:copy(
                                   filename:join([ParisCacheDir, ?TEMPLATES_DIR, File]), 
                                   filename:join([RebarTemplateDir, File])) of
                              {ok, _} -> 
                                paris_log:info("* Install ~s...", [File]);
                              {error, _} -> 
                                paris_log:err("! Can't install ~s...", [File])
                            end
                        end, Files);
        _ ->
          paris_log:err("! Can't find new templates")
      end;
    _ ->
      ok
  end.

update_plugins(Options, ParisCacheDir, ParisPluginsDir) ->
  case elists:include(Options, plugins) orelse not elists:include(Options, templates) of
    true ->
      paris_log:info("* Update plugins..."),
      case file:list_dir_all(filename:join([ParisCacheDir, ?PLUGINS_DIR])) of
        {ok, Files} -> lists:foreach(fun(File) ->
                                         case file:copy(
                                                filename:join([ParisCacheDir, ?PLUGINS_DIR, File]), 
                                                filename:join([ParisPluginsDir, File])) of
                                           {ok, _} -> 
                                             paris_log:info("* Install ~s...", [File]);
                                           {error, _} -> 
                                             paris_log:err("! Can't install ~s...", [File])
                                         end
                                     end, Files);
        _ -> 
          paris_log:err("! Can't find new plugins...", [])
      end;
    _ ->
      ok
  end.

update_paris(ParisCacheDir) ->
  case file:copy(
      filename:join([ParisCacheDir, "paris"]),
      paris:get_script()) of
    {ok, _} -> 
      file:change_mode(paris:get_script(), 8#00770),
      paris_log:info("* Replace ~s", [paris:get_script()]);
    {error, _} ->
      paris_log:err("! Can't update ~s", [paris:get_script()])
  end.

need_update(ParisCache, TemplateDir) ->
  ParisTemplateFile = filename:join([TemplateDir, "paris.template"]),
  case filelib:is_regular(ParisTemplateFile) of
    false -> true;
    true -> lists:any(fun(TagVersion) ->
            semver:from_str(paris:get_version()) < semver:from_str(TagVersion)
        end, git:tags(ParisCache))
  end.

