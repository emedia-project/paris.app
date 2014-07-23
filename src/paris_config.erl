-module(paris_config).

-export([
  texas_add_table/2
  ]).
-include("paris.hrl").

texas_add_table(AppName, TableName) ->
  push([texas, tables], AppName, TableName).

push(Path, AppName, Data) ->
  ConfigFile = filename:join(["config", AppName ++ ".config"]),
  case erlconf:open(list_to_atom(ConfigFile), ConfigFile) of
    {ok, PID} -> 
      [Term] = erlconf:term(PID),
      case do_push(Path, Term, Data) of
        error -> {error, "Wrong path"};
        NewTerm -> 
          erlconf:term(PID, [NewTerm]),
          case erlconf:close(PID) of
            {error, _} -> {error, "Update config failed"};
            close -> ok
          end
      end;
    {error, _} -> {error, "Can't open config file"}
  end.

do_push([], Term, Data) -> 
  case paris_utils:include(Data, Term) of
    true -> Term;
    false -> lists:reverse([Data|lists:reverse(Term)])
  end;
do_push([Node|Rest], Term, Data) ->
  case lists:keyfind(Node, 1, Term) of
    {Node, SubTerm} -> 
      case do_push(Rest, SubTerm, Data) of 
        error -> error;
        UpdatedTerm -> 
          lists:keyreplace(Node, 1, Term, {Node, UpdatedTerm})
      end;
    _ -> error
  end.
