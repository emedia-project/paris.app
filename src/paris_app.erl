-module(paris_app).

-export([
  is_app/0
  ]).
-include("paris.hrl").

is_app() ->
  case filelib:is_dir("apps") of
    false -> false;
    true -> 
      case lists:foldl(fun(App, Apps) ->
              AppName = filename:basename(App),
              case lists:all(fun(Src) ->
                      filelib:is_dir(filename:join([App, "src", Src]))
                  end, ["controller", "view", "model"]) of
                false -> Apps;
                true -> 
                  case filelib:is_regular(filename:join(["config", AppName ++ ".config"])) of
                    false -> Apps;
                    true -> [AppName|Apps]
                  end
              end
          end, [], filelib:wildcard(filename:join(["apps", "*"]))) of
        [] -> false;
        L -> {true, L}
      end
  end.

