-module(paris_rebar).

-export([
  run/1,
  build_params/1,
  build_params/2
  ]).

run(Commands) ->
  rebar:main(Commands).

build_params(Params) when is_list(Params) ->
  build_params(Params, []).
build_params([], Result) when is_list(Result) -> 
  lists:map(fun({Key, Value}) ->
        atom_to_list(Key) ++ "=" ++ Value
    end, Result);
build_params([Param|Rest], Result) when is_list(Result) ->
  {Key, Value} = paris_utils:split_first(Param, "="),
  build_params(Rest, eutils:merge_keylists(
      1, 
      [{list_to_atom(string:strip(Key)), string:strip(Value)}],
      Result
      )).
