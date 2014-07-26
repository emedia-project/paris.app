-module(paris_config).

-export([
  texas_add_table/2,
  plugins_list/1,
  plugins_add/2,
  plugin_remove/2
  ]).
-include("paris.hrl").

texas_add_table(AppName, TableName) ->
  list_add([texas, tables], AppName, TableName).

plugins_list(AppName) ->
  get_element([list_to_atom(AppName), plugins], AppName).

plugins_add(AppName, PluginName) ->
  list_add([list_to_atom(AppName), plugins], AppName, PluginName).

plugin_remove(AppName, PluginName) ->
  ok.

get_element(Path, AppName) ->
  case open_config(AppName) of
    {ok, PID} -> 
      [Term] = erlconf:term(PID),
      Result = do_get_element(Path, Term),
      erlconf:close(PID),
      Result;
    {error, _} -> ok
  end.
do_get_element([], Term) -> {ok, Term};
do_get_element([Node|Rest], Term) ->
  case lists:keyfind(Node, 1, Term) of
    {Node, SubTerm} -> do_get_element(Rest, SubTerm);
    _ -> {error, "Wrong Path"}
  end.

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
  case paris_utils:include(Data, Term) of
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

