-module(paris_update).

-export([run/1, help/0, ready/0]).
-include("paris.hrl").

run(Params) ->
  update(paris_utils:include("--force", Params)).

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s update [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --force          : Force to reinstall the last version", []).

ready() ->
  RebarTemplateFile = filename:join([os:getenv("HOME"), ".rebar", "templates", "paris.template"]),
  filelib:is_regular(RebarTemplateFile).

update(Force) ->
  ?CONSOLE("* Fetch templates versions...", []),
  RebarTemplatesDir = filename:join([os:getenv("HOME"), ".rebar", "templates"]),
  RebarTemplatesCache = filename:join([os:getenv("HOME"), ".paris", ".app.cache"]),
  case paris_utils:make_dir(RebarTemplatesDir) of
    {error, _} -> ?CONSOLE("[E] Can't create ~s", [RebarTemplatesDir]);
    ok -> 
      case paris_utils:make_dir(RebarTemplatesCache) of
        {error, _} -> ?CONSOLE("[E] Can't create ~s", [RebarTemplatesCache]);
        ok ->
          case git:clone(?GIT_TEMPLATE_URL, RebarTemplatesCache) of
            {ok, _} ->
              case Force or need_update(RebarTemplatesDir, RebarTemplatesCache) of
                true -> perform_update(RebarTemplatesDir, RebarTemplatesCache);
                false -> ?CONSOLE("[I] Already up-to-date", [])
              end;
            _ -> ?CONSOLE("[E] Can't clone ~p", [?GIT_TEMPLATE_URL])
          end,
          paris_utils:remove_recursive(RebarTemplatesCache)
      end
  end.

need_update(RebarTemplatesDir, RebarTemplatesCache) -> 
  ParisTemplateFile = filename:join([RebarTemplatesDir, "paris.template"]),
  case filelib:is_regular(ParisTemplateFile) of
    false -> true;
    true -> lists:any(fun(TagVersion) ->
            semver:from_str(paris:get_version()) < semver:from_str(TagVersion)
        end, git:tags(RebarTemplatesCache))
  end.
perform_update(RebarTemplatesDir, RebarTemplatesCache) ->
  ?CONSOLE("* Update templates...", []),
  {ok, []} = case paris_utils:fmax(fun semver:from_str/1, git:tags(RebarTemplatesCache)) of
    error -> 
      ?CONSOLE("* => master", []),
      git:checkout(RebarTemplatesCache, "master");
    Version -> 
      ?CONSOLE("* => ~s", [Version]),
      git:checkout(RebarTemplatesCache, Version)
  end,
  lists:foreach(fun(File) ->
        case file:delete(File) of
          ok -> ?CONSOLE("  * Delete old file ~s", [filename:basename(File)]);
          _ -> ?CONSOLE("  [W] Can't delete file ~s", [filename:basename(File)])
        end
    end, filelib:wildcard(filename:join([RebarTemplatesDir, "paris*"]))),
  case file:list_dir_all(filename:join([RebarTemplatesCache, ?TEMPLATES_DIR])) of
    {ok, Files} -> lists:foreach(fun(File) ->
            case file:copy(
                filename:join([RebarTemplatesCache, ?TEMPLATES_DIR, File]), 
                filename:join([RebarTemplatesDir, File])) of
              {ok, _} -> ?CONSOLE("  * Install ~s...", [File]);
              {error, _} -> ?CONSOLE("  [E] Can't install ~s...", [File])
            end
        end, Files);
    _ -> ?CONSOLE("  [E] Can't find templates...", [])
  end,
  case file:copy(
      filename:join([RebarTemplatesCache, "paris"]),
      paris:get_script()) of
    {ok, _} -> 
      file:change_mode(paris:get_script(), 8#00770),
      ?CONSOLE("* Update ~s", [paris:get_script()]);
    {error, _} ->
      ?CONSOLE("  [E] Can't update ~s", [paris:get_script()])
  end.

