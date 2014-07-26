-module(paris_update).

-export([run/1, help/0]).
-export([need_update/2]).
-include("paris.hrl").

run(Params) ->
  {InstallTemplates, InstallPlugins} = case {paris_param:exist("--templates", Params),
                                             paris_param:exist("--plugins", Params)} of
    {false, false} -> {true, true};
    E -> E
  end,
  update(
    paris_param:exist("--master", Params),
    paris_param:exist("--force", Params), 
    InstallTemplates, 
    InstallPlugins
    ).

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s update [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --master         : Update from master branch (AT YOUR OWN RISKS)", []),
  ?CONSOLE("     --force          : Force to reinstall the last version", []),
  ?CONSOLE("     --templates      : Update templated only", []),
  ?CONSOLE("     --plugins        : Update plugins only", []),
  ?CONSOLE("     --all            : Update templates and plugins (default)", []).

update(Master, Force, InstallTemplates, InstallPlugins) ->
  case paris_install:prepare() of
    {ok, RebarTemplatesDir, ParisPluginsDir, ParisCache} -> 
      case Force or need_update(RebarTemplatesDir, ParisCache) of
        true -> perform_update(Master, RebarTemplatesDir, ParisPluginsDir, ParisCache, InstallTemplates, InstallPlugins);
        false -> ?CONSOLE("[I] Already up-to-date", [])
      end,
      paris_utils:remove_recursive(ParisCache);
    {error, Error} -> ?CONSOLE("  [E] ~s", [Error])
  end.

need_update(RebarTemplatesDir, ParisCache) -> 
  ParisTemplateFile = filename:join([RebarTemplatesDir, "paris.template"]),
  case filelib:is_regular(ParisTemplateFile) of
    false -> true;
    true -> lists:any(fun(TagVersion) ->
            semver:from_str(paris:get_version()) < semver:from_str(TagVersion)
        end, git:tags(ParisCache))
  end.

perform_update(Master, RebarTemplatesDir, ParisPluginsDir, ParisCache, Templates, Plugins) ->
  UseVersion = if
    Master -> "master";
    true ->
      case paris_utils:fmax(fun semver:from_str/1, git:tags(ParisCache)) of
        error -> "master";
        Version -> Version
      end
  end,
  ?CONSOLE("* Update to version ~s", [UseVersion]),
  {ok, []} = git:checkout(ParisCache, UseVersion),
  if
    Templates =:= true -> paris_install:install_templates(RebarTemplatesDir, ParisCache);
    true -> ok
  end,
  if 
    Plugins =:= true -> paris_install:install_plugins(ParisPluginsDir, ParisCache);
    true -> ok
  end,
  case file:copy(
      filename:join([ParisCache, "paris"]),
      paris:get_script()) of
    {ok, _} -> 
      file:change_mode(paris:get_script(), 8#00770),
      ?CONSOLE("* Update ~s", [paris:get_script()]);
    {error, _} ->
      ?CONSOLE("  [E] Can't update ~s", [paris:get_script()])
  end.

