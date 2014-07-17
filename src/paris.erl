-module(paris).

-export([main/1]).
-include("paris.hrl").

main(Args) ->
  _ = application:start(paris),
  run(Args).

run([]) ->
  help();
run(["--help"|_]) ->
  help();
run(["-h"|_]) ->
  help();
run(["--version"|_]) ->
  version();
run(["new", AppName|Params]) ->
  paris_app:create(AppName, Params);
run(["--rebar"|Rest]) ->
  paris_rebar:run(Rest);
run(["--update"|Rest]) ->
  update(paris_utils:include("--force", Rest));
run(["-u"|Rest]) ->
  update(paris_utils:include("--force", Rest));
run(_) ->
  run([]).

version() ->
  ?CONSOLE("~s ~s", [filename:basename(escript:script_name()), get_version()]). 

get_version() ->
  case application:get_key(paris, vsn) of
    {ok, Vsn} -> Vsn;
    _ -> "0.0.0"
  end.

help() ->
  version().

update(Force) ->
  ?CONSOLE("* Fetch templates versions...", []),
  RebarTemplatesDir = filename:join([os:getenv("HOME"), ".rebar", "templates"]),
  RebarTemplatesCache = filename:join([RebarTemplatesDir, ".cache"]),
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
            semver:from_str(get_version()) < semver:from_str(TagVersion)
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
  end.
  
