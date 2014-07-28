-module(paris_params).

-export([
    exist/2,
    value/2,
    value/3
  ]).

exist(Name, Params) ->
  elists:include(Name, Params).

value(Name, Params) ->
  value(Name, undefined, Params).

value(_, Default, []) -> Default;
value(Name, _, [Name, Value|_]) -> Value;
value(Name, Default, [_|Params]) -> value(Name, Default, Params).



