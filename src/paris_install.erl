-module(paris_install).

-export([run/1, help/0]).
-export([prepare/0, install_templates/2, install_plugins/2, ready/0]).
-include("paris.hrl").

run(Params) ->
  {InstallTemplates, InstallPlugins} = case {paris_utils:include("--templates", Params),
                                             paris_utils:include("--plugins", Params)} of
    {false, false} -> {true, true};
    E -> E
  end,
  install(InstallTemplates, InstallPlugins).

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s install [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --templates      : Install templated only", []),
  ?CONSOLE("     --plugins        : Install plugins only", []),
  ?CONSOLE("     --all            : Install templates and plugins (default)", []).

ready() ->
  RebarTemplateFile = filename:join([os:getenv("HOME"), ".rebar", "templates", "paris.template"]),
  filelib:is_regular(RebarTemplateFile).

install(Templates, Plugins) ->
  case prepare() of
    {error, Error} -> ?CONSOLE("  [E] ~s", [Error]);
    {ok, RebarTemplatesDir, ParisPluginsDir, ParisCache} ->
      Version = paris:get_version(),
      ?CONSOLE("* Use version ~s", [Version]),
      {ok, []} = git:checkout(ParisCache, Version),
      if
        Templates =:= true -> install_templates(RebarTemplatesDir, ParisCache);
        true -> ok
      end,
      if 
        Plugins =:= true -> install_plugins(ParisPluginsDir, ParisCache);
        true -> ok
      end,
      case paris_update:need_update(RebarTemplatesDir, ParisCache) of
        true -> ?CONSOLE("[I] Update available. Run `~s update'", [paris:get_script_name()]);
        false -> ok
      end,
      paris_utils:remove_recursive(ParisCache)
  end.

prepare() ->
  ?CONSOLE("* Fetch templates and plugins...", []),
  RebarTemplatesDir = filename:join([os:getenv("HOME"), ".rebar", "templates"]),
  ParisPluginsDir = filename:join([os:getenv("HOME"), ".paris", "plugins"]),
  ParisCache = filename:join([os:getenv("HOME"), ".paris", ".app.cache"]),
  case paris_utils:make_dir(RebarTemplatesDir) of
    {error, _} -> {error, "Can't create " ++ RebarTemplatesDir};
    ok -> 
      case paris_utils:make_dir(ParisCache) of
        {error, _} -> {error, "Can't create " ++ ParisCache};
        ok ->
          case paris_utils:make_dir(ParisPluginsDir) of
            {error, _} -> {error, "Can't create " ++ ParisPluginsDir};
            ok ->
              case git:clone(?GIT_TEMPLATE_URL, ParisCache) of
                {ok, _} -> {ok, RebarTemplatesDir, ParisPluginsDir, ParisCache};
                _ -> {error, "Can't retrieve data"}
              end
          end
      end
  end.

install_templates(RebarTemplatesDir, ParisCache) ->
  ?CONSOLE("* Install templates...", []),
  lists:foreach(fun(File) ->
        case file:delete(File) of
          ok -> ?CONSOLE("  * Delete old file ~s", [filename:basename(File)]);
          _ -> ?CONSOLE("  [W] Can't delete file ~s", [filename:basename(File)])
        end
    end, filelib:wildcard(filename:join([RebarTemplatesDir, "paris*"]))),
  case file:list_dir_all(filename:join([ParisCache, ?TEMPLATES_DIR])) of
    {ok, Files} -> lists:foreach(fun(File) ->
            case file:copy(
                filename:join([ParisCache, ?TEMPLATES_DIR, File]), 
                filename:join([RebarTemplatesDir, File])) of
              {ok, _} -> ?CONSOLE("  * Install ~s...", [File]);
              {error, _} -> ?CONSOLE("  [E] Can't install ~s...", [File])
            end
        end, Files);
    _ -> ?CONSOLE("  [E] Can't find templates...", [])
  end.

install_plugins(ParisPluginsDir, ParisCache) -> 
  ?CONSOLE("* Install plugins...", []),
  case file:list_dir_all(filename:join([ParisCache, ?PLUGINS_DIR])) of
    {ok, Files} -> lists:foreach(fun(File) ->
            case file:copy(
                filename:join([ParisCache, ?PLUGINS_DIR, File]), 
                filename:join([ParisPluginsDir, File])) of
              {ok, _} -> ?CONSOLE("  * Install ~s...", [File]);
              {error, _} -> ?CONSOLE("  [E] Can't install ~s...", [File])
            end
        end, Files);
    _ -> ?CONSOLE("  [E] Can't find plugins...", [])
  end.
