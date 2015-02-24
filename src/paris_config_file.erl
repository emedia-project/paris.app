-module(paris_config_file).

-export([
         texas_add_table/2
        ]).

texas_add_table(AppName, TableName) ->
  list_add([texas, tables], AppName, TableName).

list_add(Path, AppName, Data) ->
  case open_config(AppName) of
    {ok, PID} -> 
      [Term] = erlconf:term(PID),
      case do_list_add(Path, Term, Data) of
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
do_list_add([], Term, Data) -> 
  case elists:include(Term, Data) of
    true -> Term;
    false -> lists:reverse([Data|lists:reverse(Term)])
  end;
do_list_add([Node|Rest], Term, Data) ->
  {Node, SubTerm} = case lists:keyfind(Node, 1, Term) of
    false -> {Node, []};
    T -> T
  end,
  UpdatedTerm = do_list_add(Rest, SubTerm, Data),
  lists:keystore(Node, 1, Term, {Node, UpdatedTerm}).

open_config(AppName) ->
  ConfigFile = filename:join(["config", AppName ++ ".config"]),
  erlconf:open(list_to_atom(ConfigFile), ConfigFile).

