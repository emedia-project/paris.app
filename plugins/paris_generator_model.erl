-module(paris_generator_model).
  
-export([generate/1]).
-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).
-define(DEFAULT_STRING_LENGTH, 256).

generate(["--help"|_]) -> help();
generate([ModelName|Params]) ->
  case paris_app:is_app() of
    {true, [AppName]} -> 
      ?CONSOLE("* Model `~s'", [ModelName]),
      ModelParams = get_params(Params),
      ModelParams1 = case lists:keyfind(id, 1, ModelParams) of
        false -> [{id, [{type, id}, {autoincrement, true}]}|ModelParams];
        _ -> ModelParams
      end,
      case build_model(AppName, ModelName, ModelParams1) of
        {ok, ModelFile} -> ?CONSOLE("> create ~s", [ModelFile]);
        {error, ModelFile} -> ?CONSOLE("[E] Can't create ~s", [ModelFile])
      end,
      case paris_config:texas_add_table(AppName, list_to_atom(inflector:singularize(ModelName))) of
        ok -> ?CONSOLE("> Register table for ~s", [ModelName]);
        {error, Error} -> ?CONSOLE("[E] Can't register table : ~s", [Error])
      end;
    {true, _} -> ?CONSOLE("[W] Can't create model in a multi application!", []);
    false -> ?CONSOLE("[E] Not a paris application!", [])
  end.

help() ->
  ?CONSOLE("Usage:", []),
  ?CONSOLE("~s generate model [params] [options]", [paris:get_script_name()]),
  ?CONSOLE("~nOptions:~n", []),
  ?CONSOLE("     --help           : Display this help", []),
  ?CONSOLE("~nExample:~n", []),
  ?CONSOLE("  paris generate model posts", []),
  ?CONSOLE("  paris generate model posts title:string:256 body:text", []),
  ?CONSOLE("  paris generate model posts title body:text", []).

get_params(Params) ->
  get_params(Params, []).

get_params([], Result) -> Result;
get_params([Current|Rest], Result) ->
  Param = case estring:split_first(Current, ":") of
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

build_model(AppName, ModelName, Params) ->
  ParamsString = lists:map(fun(Param) ->
          io_lib:format("~p", [Param])
      end, Params),
  ModelFile = filename:join(
                ["apps", AppName, 
                 "src", "models", 
                 inflector:singularize(ModelName) ++ ".erl"]),
  case model_dtl:render(
         [{name, inflector:singularize(ModelName)}, 
          {columns, ParamsString}]) of
    {ok, Data} -> 
      case file:write_file(ModelFile, Data) of
        ok -> {ok, ModelFile};
        {error, _} -> {error, ModelFile}
      end;
    _ -> 
      {error, ModelFile}
  end.
