-module(paris_rebar).

-export([
         run/1,
         params/1
        ]).

run(Commands) ->
  rebar:main(Commands).

params(Params) when is_list(Params) ->
  lists:map(fun({Key, Value}) ->
        atom_to_list(Key) ++ "=" ++ Value
    end, Params).

