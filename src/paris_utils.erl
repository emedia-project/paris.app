-module(paris_utils).

-export([
  split_first/2,
  include/2,
  make_dir/1,
  remove_recursive/1,
  fmax/2,
  module_exist/1
  ]).

split_first(String, Token) ->
  split_first(String, Token, {[], []}).
split_first([], _, R) -> R;
split_first([C|Rest], Token, {A, B}) ->
  case include(C, Token) of
    true -> {A, Rest};
    false -> split_first(Rest, Token, {A ++ [C], B})
  end.

include(X, List) ->
  lists:any(fun(E) -> E =:= X end, List).

make_dir(Path) ->
  filelib:ensure_dir(filename:join([Path, "."])).

remove_recursive(Path) ->
  case filelib:is_dir(Path) of
    false ->
      file:delete(Path);
    true ->
      lists:foreach(fun remove_recursive/1, sub_files(Path)),
      file:del_dir(Path)
  end.

sub_files(From) ->
  {ok, SubFiles} = file:list_dir(From),
  [filename:join(From, SubFile) || SubFile <- SubFiles].

fmax(_, []) -> error;
fmax(Fun, List) ->
  [First|Rest] = List,
  lists:foldl(fun(Element, Max) ->
        case Fun(Element) > Fun(Max) of
          true -> Element;
          false -> Max
        end
    end, First, Rest).

module_exist(Module) ->
  case is_atom(Module) of
    true ->
      try Module:module_info() of
        _InfoList ->
          true
      catch
        _:_ ->
          false
      end;
    false ->
      false
  end.
