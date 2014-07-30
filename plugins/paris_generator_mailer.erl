-module(paris_generator_mailer).
  
-export([generate/1]).
-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

generate(["--help"|_]) -> help();
generate([Name|_]) ->
  case paris_app:is_app() of
    {true, [AppName]} -> 
      ?CONSOLE("* Create mailer `~s'", [Name]),
      lists:map(fun({Template, Output}) ->
            case Template:render([{appname, AppName}, {mailer, Name}]) of
              {ok, Data} ->
                case file:write_file(Output, Data) of
                  ok -> 
                    ?CONSOLE("  > create ~s", [Output]);
                  {error, _} ->
                    ?CONSOLE("[E] faild to create ~s", [Output])
                end;
              {error, _} ->
                ?CONSOLE("[E] faild to create ~s", [Output])
            end
        end, [
          {mailer_view_html_dtl, 
           filename:join(["apps", AppName, "src", "views", Name ++ ".html"])},
          {mailer_view_txt_dtl, 
           filename:join(["apps", AppName, "src", "views", Name ++ ".txt"])},
          {mailer_mailer_dtl, 
           filename:join(["apps", AppName, "src", "mailers", Name ++ ".erl"])}
          ]);
    {true, _} -> ?CONSOLE("[W] Can't scaffold in a multi application!", []);
    false -> ?CONSOLE("[E] Not a paris application!", [])
  end.

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s generate mailer [params] [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --help           : Display this help", []),
  ?CONSOLE("~nExample:~n", []),
  ?CONSOLE("  paris generate mailer my_mailer", []).
