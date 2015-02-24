-module(paris_generator_controller).

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
  { controller, "Generator to create controller"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate controller <name>").

g(_Config, Options, [Name|_]) ->
  Name1 = inflector:underscore(eutils:to_string(Name)),
  case paris_app:is_app() of
    false ->
      paris_log:stop("! Not a paris application");
    {true, [AppName]} ->
      paris_log:info("* Create controller ~s", [Name1]),
      lists:foreach(fun({File, Template}) ->
                        create_file(AppName, 
                                    Name1, 
                                    File, 
                                    Options, Template)
                    end, [{filename:join(["apps", AppName, "src", "controllers", Name1 ++ ".erl"]),
                           paris_generator_controller_ctrl_dtl},
                          {filename:join(["apps", AppName, "src", "views", Name1 ++ ".html"]),
                           paris_generator_controller_view_dtl}]);
    {true, _} ->
      paris_log:stop("! Can't create controller in a multi application")
  end.

create_file(AppName, Name, File, Options, Template) ->
  case paris_pv_plugins_generate:generate_status(File, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [File]);
    Status ->
      case Template:render([{appname, AppName}, {name, Name}, {file, File}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
          file:write_file(File, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [File])
      end
  end.
