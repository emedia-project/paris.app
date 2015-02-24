-module(paris_generator_mailer).

-export([
         generate/3,
         generator_info/0
        ]).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse Args =:= []) of
    true -> help(Config);
    false -> g(Config, Options, Args)
  end.

generator_info() ->
  { mailer, "Generator to create mailer"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate mailer <params>").

g(_Config, Options, [Name|_]) ->
  case paris_app:is_app() of
    {true, [AppName]} -> 
      Name1 = inflector:underscore(eutils:to_string(Name)),
      paris_log:info("* Create mailer `~s'", [Name1]),
      lists:map(fun({Template, File}) ->
                    case paris_pv_plugins_generate:generate_status(File, Options) of
                      skip ->
                        paris_log:debug("* Skip file ~s: already exist", [File]);
                      Status ->
                        case Template:render([{appname, AppName}, {mailer, Name1}]) of
                          {ok, Data} ->
                            paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
                            file:write_file(File, Data);
                          _ ->
                            paris_log:stop("! Can't create ~s", [File])
                        end
                    end
                end, [
                      {paris_generator_mailer_view_html_dtl, 
                       filename:join(["apps", AppName, "src", "views", Name1 ++ ".html"])},
                      {paris_generator_mailer_view_txt_dtl, 
                       filename:join(["apps", AppName, "src", "views", Name1 ++ ".txt"])},
                      {paris_generator_mailer_dtl, 
                       filename:join(["apps", AppName, "src", "mailers", Name1 ++ ".erl"])}
                     ]);
    {true, _} -> 
      paris_log:stop("! Can't create mailer in a multi application");
    false ->
      paris_log:stop("! Not a paris application")
  end.
