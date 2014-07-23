-module(paris_generator_scaffold).
  
-export([generate/1]).
-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).
-define(DEFAULT_STRING_LENGTH, 256).

generate(["--help"|_]) -> help();
generate([ModelName|Params]) ->
  case paris_app:is_app() of
    {true, [AppName]} -> 
      ?CONSOLE("* Scaffold `~s'", [ModelName]),
      ModelParams = get_params(Params),
      ModelParams1 = case lists:keyfind(id, 1, ModelParams) of
        false -> [{id, [{type, id}, {autoincrement, true}]}|ModelParams];
        _ -> ModelParams
      end,
      case build_model(AppName, ModelName, ModelParams1) of
        {ok, ModelFile} -> ?CONSOLE("> create ~s", [ModelFile]);
        {error, ModelFile} -> ?CONSOLE("[E] Can't create ~s", [ModelFile])
      end,
      case build_controller(AppName, ModelName, ModelParams) of
        {ok, CtrlFile} -> ?CONSOLE("> create ~s", [CtrlFile]);
        {error, CtrlFile} -> ?CONSOLE("[E] Can't create ~s", [CtrlFile])
      end,
      lists:foreach(fun(View) ->
            case build_views(atom_to_list(View), AppName, ModelName, ModelParams) of
              {ok, ViewFile} -> ?CONSOLE("> create ~s", [ViewFile]);
              {error, ViewFile} -> ?CONSOLE("[E] Can't create ~s", [ViewFile])
            end
        end, [layout, form, index, new, show, edit]),
      case paris_config:texas_add_table(AppName, list_to_atom(inflector:singularize(ModelName))) of
        ok -> ?CONSOLE("> Register table for ~s", [ModelName]);
        {error, Error} -> ?CONSOLE("[E] Can't register table : ~s", [Error])
      end;
    {true, _} -> ?CONSOLE("[W] Can't scaffold in a multi application!", []);
    false -> ?CONSOLE("[E] Not a paris application!", [])
  end.

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s generate scaffold <name> [params] [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --help           : Display this help", []),
  ?CONSOLE("~nExample:~n", []),
  ?CONSOLE("  paris generate scaffold posts", []),
  ?CONSOLE("  paris generate scaffold posts title:string:256 body:text", []),
  ?CONSOLE("  paris generate scaffold posts title body:text", []).

get_params(Params) ->
  get_params(Params, []).

get_params([], Result) -> Result;
get_params([Current|Rest], Result) ->
  Param = case paris_utils:split_first(Current, ":") of
    {Key, []} -> {list_to_atom(Key), [{type, string}, {len, ?DEFAULT_STRING_LENGTH}]};
    {Key, Value} -> 
      case paris_utils:split_first(Value, ":") of
        {Type, []} -> 
          case list_to_atom(Type) of
            string -> {list_to_atom(Key), [{type, string}, {len, ?DEFAULT_STRING_LENGTH}]};
            T -> {list_to_atom(Key), [{type, T}]}
          end;
        {Type, Len} -> {list_to_atom(Key), [{type, list_to_atom(Type)}, {len, list_to_integer(Len)}]}
      end
  end,
  get_params(Rest, [Param|Result]).

build_model(AppName, ModelName, Params) ->
  ParamsString = lists:map(fun(Param) ->
          io_lib:format("~p", [Param])
      end, Params),
  ModelFile = filename:join(["apps", AppName, "src", "model", inflector:singularize(ModelName) ++ ".erl"]),
  case scaffold_model_dtl:render([{name, inflector:singularize(ModelName)}, {columns, ParamsString}]) of
    {ok, Data} -> 
      case file:write_file(ModelFile, Data) of
        ok -> {ok, ModelFile};
        {error, _} -> {error, ModelFile}
      end;
    _ -> 
      {error, ModelFile}
  end.

build_controller(AppName, ModelName, Params) ->
  ControllerFile = filename:join(["apps", AppName, "src", "controller", inflector:pluralize(ModelName) ++ ".erl"]),
  case scaffold_controller_dtl:render([
        {ctrl_name, inflector:pluralize(ModelName)},
        {model_name, inflector:singularize(ModelName)},
        {columns, get_params_with_type(Params)}
        ]) of
    {ok, Data} ->
      case file:write_file(ControllerFile, Data) of
        ok -> {ok, ControllerFile};
        {error, _} -> {error, ControllerFile}
      end;
    _ ->
      {error, ControllerFile}
  end.

build_views(View, AppName, ModelName, Params) ->
  ViewFile = filename:join(["apps", AppName, "src", "view", inflector:pluralize(ModelName) ++ "_" ++ View ++ ".html"]),
  ViewTemplate = list_to_atom("scaffold_view_" ++ View ++ "_dtl"),
  case ViewTemplate:render([
        {ctrl_name, inflector:pluralize(ModelName)},
        {model_name, inflector:singularize(ModelName)},
        {columns, get_params_with_type(Params)}
        ]) of
    {ok, Data} ->
      case file:write_file(ViewFile, Data) of
        ok -> {ok, ViewFile};
        {error, _} -> {error, ViewFile}
      end;
    _ ->
      {error, ViewFile}
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
