-module(paris_generator_scaffold).

-export([
         generate/3,
         generator_info/0
        ]).
-define(DEFAULT_STRING_LENGTH, 256).

generate(Config, Options, Args) ->
  case (elists:include(Options, help) orelse length(Args) < 2) of
    true -> help(Config);
    false -> g(Config, Options, Args)
  end.

generator_info() ->
  { scaffold, "Scaffolds an entire resource"}.

help(Config) ->
  paris_plugins:help(generate, Config, "generate scaffold <name> [field[:type[:index]] ...]"),
  paris_log:print("Example:~n", []),
  paris_log:print("  paris generate scaffold posts", []),
  paris_log:print("  paris generate scaffold posts title:string:256 body:text", []),
  paris_log:print("  paris generate scaffold posts title body:text", []).

g(_Config, Options, [Name|Params]) ->
  case paris_app:is_app() of
    {true, [AppName]} ->
      paris_log:info("* Scaffold ~s", [Name]),
      ModelParams = get_params(Params),
      build_model(AppName, Name, ModelParams, Options),
      build_controller(AppName, Name, ModelParams, Options),
      lists:foreach(fun(View) ->
                        build_view(View, AppName, Name, ModelParams, Options)
                    end, [layout, form, index, new, show, edit]),
      case paris_config_file:texas_add_table(AppName, 
                                             eutils:to_atom(
                                               inflector:singularize(
                                                 eutils:to_string(Name)))) of
        ok -> 
          paris_log:info("* Register table ~s", [Name]);
        {error, Error} -> 
          paris_log:stop("! Can't register table ~s: ~s", [Name, Error])
      end;
    {true, _} ->
      paris_log:stop("! Can't create controller in a multi application");
    false ->
      paris_log:stop("! Not a paris application")
  end.

get_params(Params) -> get_params(Params, []).

get_params([], Result) ->
  case lists:keyfind(id, 1, Result) of
    false -> [{id, [{type, id}, {autoincrement, true}]}|Result];
    _ -> Result
  end;
get_params([Current|Rest], Result) ->
  Param = case estring:split_first(eutils:to_string(Current), ":") of
            {Key, []} -> {list_to_atom(Key), [{type, string}, {len, ?DEFAULT_STRING_LENGTH}]};
            {Key, Value} -> 
              case estring:split_first(Value, ":") of
                {Type, []} -> 
                  case list_to_atom(Type) of
                    string -> {list_to_atom(Key), [{type, string}, {len, ?DEFAULT_STRING_LENGTH}]};
                    T -> {list_to_atom(Key), [{type, T}]}
                  end;
                {Type, Len} -> {list_to_atom(Key), [{type, list_to_atom(Type)}, {len, list_to_integer(Len)}]}
              end
          end,
  get_params(Rest, [Param|Result]).


build_model(AppName, Name, Params, Options) ->
  Name1 = inflector:singularize(eutils:to_string(Name)),
  File = filename:join(["apps", AppName, "src", "models", Name1 ++ ".erl"]),
  case paris_pv_plugins_generate:generate_status(File, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [File]);
    Status ->
      case paris_generator_scaffold_model_dtl:render(
             [{name, Name1}, 
              {columns, lists:map(fun(Param) ->
                                      io_lib:format("~p", [Param])
                                  end, Params)}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
          file:write_file(File, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [File])
      end
  end.

build_controller(AppName, Name, Params, Options) ->
  Name1 = inflector:pluralize(eutils:to_string(Name)),
  File = filename:join(["apps", AppName, "src", "controllers", Name1 ++ ".erl"]),
  case paris_pv_plugins_generate:generate_status(File, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [File]);
    Status ->
      case paris_generator_scaffold_controller_dtl:render(
             [{ctrl_name, Name1},
              {model_name, inflector:singularize(eutils:to_string(Name))},
              {columns, get_params_with_type(Params)}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
          file:write_file(File, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [File])
      end
  end.

build_view(View, AppName, Name, Params, Options) -> 
  Name1 = inflector:pluralize(eutils:to_string(Name)),
  File = filename:join(["apps", AppName, "src", "views", Name1 ++ "_" ++ eutils:to_string(View) ++ ".html"]),
  ViewTemplate = eutils:to_atom("paris_generator_scaffold_view_" ++ eutils:to_string(View) ++ "_dtl"),
  case paris_pv_plugins_generate:generate_status(File, Options) of
    skip ->
      paris_log:debug("* Skip file ~s: already exist", [File]);
    Status ->
      case ViewTemplate:render(
             [{ctrl_name, Name1},
              {model_name, inflector:singularize(eutils:to_string(Name))},
              {columns, get_params_with_type(Params)}]) of
        {ok, Data} ->
          paris_log:info("* ~s ~s", [estring:capitalize(eutils:to_string(Status)), File]),
          file:write_file(File, Data);
        _ ->
          paris_log:stop("! Can't create ~s", [File])
      end
  end.

get_params_with_type(Params) ->
  lists:map(fun({Column, Def}) ->
        case lists:keyfind(type, 1, Def) of
          {type, Type} -> 
            [{name, Column}, {type, Type}];
          _ -> 
            [{name, Column}, {type, string}]
        end
    end, Params).
