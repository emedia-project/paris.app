-module(paris_config).

-export([
         load/1,
         plugin_module/2,
         plugins/1,
         options/1,
         options/2,
         template_dir/1,
         git_template_url/1,
         paris_cache_dir/1,
         ready/1
        ]).
-include("paris.hrl").
-record(config, {git_template_url, 
                 template_dir, 
                 paris_plugins_dir,
                 paris_cache_dir,
                 plugins}).

load(Plugins) ->
  Conf = case file:consult(filename:join([os:getenv("HOME"), ".paris.conf"])) of
           {ok, Config} -> Config;
           {error, _} -> []
         end,
  TemplateDir = efile:expand_path(elists:keyfind(template_dir, 1, Conf, ?TEMPLATE_DIR)),
  ParisPluginsDir = efile:expand_path(filename:join([elists:keyfind(paris_dir, 1, Conf, ?PARIS_DIR), "plugins"])),
  ParisCacheDir = efile:expand_path(filename:join([elists:keyfind(paris_dir, 1, Conf, ?PARIS_DIR), ".app.cache"])),
  case efile:make_dir(TemplateDir) of
    {error, Reason} -> 
      ?HALT("! Can't create ~s: ~p", [TemplateDir, Reason]);
    ok -> 
      case efile:make_dir(ParisPluginsDir) of
        {error, Reason} ->
          ?HALT("! Can't create ~s: ~p", [ParisPluginsDir, Reason]);
        ok ->
          case efile:make_dir(ParisCacheDir) of
            {error, Reason} ->
              ?HALT("! Can't create ~s: ~p", [ParisCacheDir, Reason]);
            ok ->
              #config{
                 git_template_url = elists:keyfind(git_template_url, 1, Conf, ?GIT_TEMPLATE_URL),
                 template_dir = TemplateDir,
                 paris_plugins_dir = ParisPluginsDir,
                 paris_cache_dir = ParisCacheDir,
                 plugins = Plugins
                }
          end
      end
  end.
  
plugin_module(Command, #config{plugins = Plugins}) ->
  maps:get(Command, Plugins, undefined).

plugins(#config{plugins = Plugins}) -> Plugins.

options(#config{plugins = Plugins}) ->
  maps:fold(fun(_, #{options := Options}, Acc) ->
                Acc ++ Options
            end, [], Plugins).

options(Command, #config{plugins = Plugins}) ->
  maps:get(options, maps:get(Command, Plugins)).

template_dir(#config{template_dir = TemplateDir}) -> TemplateDir.

git_template_url(#config{git_template_url = GitTemplateUrl}) -> GitTemplateUrl.

paris_cache_dir(#config{paris_cache_dir = ParisCacheDir}) -> ParisCacheDir.

ready(Config) ->
  RebarTemplateFile = filename:join([template_dir(Config), "paris.template"]),
  filelib:is_regular(RebarTemplateFile).
